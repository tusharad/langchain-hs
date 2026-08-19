## Run HLint linter
lint:
	hlint src/ test/ langchain-hs-core/ langchain-hs-graph

## Format source code with Fourmolu
format:
	fourmolu -i src/ test/ langchain-hs-core/ langchain-hs-graph

## Check code formatting with Fourmolu
format-check:
	fourmolu -m check src/ test/ langchain-hs-core/ langchain-hs-graph

## Build Haddock documentation
docs:
	stack haddock --no-haddock-deps langchain-hs-core langchain-hs-graph langchain-hs

## Clean build artifacts
clean:
	stack clean
