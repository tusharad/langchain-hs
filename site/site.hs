{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath (takeDirectory)

main :: IO ()
main = hakyll $ do
    -- Static Assets: Images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Static Assets: CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static Assets: JavaScript
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Templates
    match "templates/*" $ compile templateBodyCompiler

    -- Landing Page
    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Documentation Pages
    match ("getting-started/*" .||. "concepts/*" .||. "guides/*" .||. "api/*") $ do
        route   $ setExtension "html"
        compile $ do
            let ctx = docContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/doc.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

-- | Custom context for documentation pages with category and title metadata
docContext :: Context String
docContext =
    field "category" extractDocCategory `mappend`
    defaultContext
  where
    extractDocCategory item = do
        let path = toFilePath (itemIdentifier item)
        let dir  = takeDirectory path
        pure $ case dir of
            "getting-started" -> "Getting Started"
            "concepts"        -> "Core Concepts"
            "guides"          -> "Guides & Recipes"
            "api"             -> "API Reference"
            _                 -> "General"
