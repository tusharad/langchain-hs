{-# LANGUAGE StrictData #-}

module Examples.DbS3StateMachine where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck

-- | Domain Types (Preconditions enforced by construction)
newtype IdempotencyKey = IdempotencyKey String deriving (Eq, Ord, Show)

newtype PayloadHash = PayloadHash String deriving (Eq, Ord, Show)
newtype S3Key = S3Key String deriving (Eq, Ord, Show)

data RowStatus = Pending | Ready | Failed deriving (Eq, Show)

-- | Pure Model of our Cloud System (Postgres Table + S3 Bucket)
data CloudModel = CloudModel
  { dbInvoices :: Map IdempotencyKey (PayloadHash, RowStatus, S3Key)
  , s3Objects :: Set S3Key
  }
  deriving (Eq, Show)

initialModel :: CloudModel
initialModel = CloudModel Map.empty Set.empty

-- | Actions representing API calls, Cron retries, and Infrastructure Faults
data Command
  = -- | Bool = Simulate S3 network failure
    UploadInvoice IdempotencyKey PayloadHash Bool
  | CronCleanupOrphans
  deriving (Eq, Show)

-- | Pure State Transition Function (The Spec / Law)
stepModel :: CloudModel -> Command -> (Either String S3Key, CloudModel)
stepModel model (UploadInvoice key hash s3Fails) =
  case Map.lookup key (dbInvoices model) of
    -- Law 1: Idempotent replay with identical hash returns existing S3Key without mutation
    Just (existingHash, Ready, s3Key)
      | existingHash == hash -> (Right s3Key, model)
    -- Law 2: Conflict detection on same key with different payload hash
    Just (existingHash, _, _)
      | existingHash /= hash -> (Left "409 Conflict", model)
    -- Fresh upload or retry of previously Failed upload
    _ ->
      let targetS3Key = S3Key ("s3://invoices/" ++ show key)
       in if s3Fails
            -- Law 3: If S3 fails, DB records Failed and S3 has no orphan object
            then
              ( Left "502 S3 Upload Failed"
              , model
                  { dbInvoices = Map.insert key (hash, Failed, targetS3Key) (dbInvoices model)
                  }
              )
            else
              ( Right targetS3Key
              , model
                  { dbInvoices = Map.insert key (hash, Ready, targetS3Key) (dbInvoices model)
                  , s3Objects = Set.insert targetS3Key (s3Objects model)
                  }
              )
stepModel model CronCleanupOrphans =
  -- Frame Condition & Cleanup Law: Only remove S3 objects not referenced by a Ready DB row
  let validS3Keys =
        Set.fromList
          [k | (_, Ready, k) <- Map.elems (dbInvoices model)]
   in (Right (S3Key "cleanup-ok"), model {s3Objects = Set.intersection (s3Objects model) validS3Keys})

-- | Global Invariant: Every 'Ready' DB row MUST have a corresponding object in S3
prop_NoDanglingDbPointers :: CloudModel -> Bool
prop_NoDanglingDbPointers model =
  all
    (\(_, status, s3Key) -> status /= Ready || Set.member s3Key (s3Objects model))
    (Map.elems (dbInvoices model))
