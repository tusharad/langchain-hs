## Run HLint linter
lint:
	hlint src/ test/ langchain-hs-core/ langchain-hs-graph

## Format source code with Fourmolu
format:
	fourmolu -i src/ test/ langchain-hs-core/ langchain-hs-graph

## Check code formatting with Fourmolu
format-check:
	fourmolu --check src/ test/ langchain-hs-core/ langchain-hs-graph

## Build Haddock documentation
docs:
	cabal haddock

## Clean build artifacts
clean:
	stack clean
