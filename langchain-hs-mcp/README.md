# langchain-hs-mcp

Model Context Protocol (MCP) client for [langchain-hs](https://hackage.haskell.org/package/langchain-hs).

Connect Haskell agents to any MCP server over stdio or HTTP JSON-RPC 2.0.
Depends only on `langchain-hs-core` — no provider dependencies.

## Installation

```yaml
# package.yaml / .cabal
dependencies:
  - langchain-hs-mcp
```

## Usage

```haskell
import Langchain.MCP.Client
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  let client = newStdioMcpClient "hackage" "docker" ["run", "-i", "--rm", "mcp/hackage-doc"]
  res <- runExceptT $ listMcpTools client
  case res of
    Left err    -> print err
    Right tools -> mapM_ (print . mcpToolName) tools
```
