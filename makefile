.PHONY: lint format format-check weeder docs site-build site-watch site-clean clean

## Run HLint linter
lint:
	hlint src/ test/ langchain-hs-core/ langchain-hs-graph examples/

## Format source code with Fourmolu
format:
	fourmolu -i src/ test/ langchain-hs-core/ langchain-hs-graph examples/

## Check code formatting with Fourmolu
format-check:
	fourmolu -m check src/ test/ langchain-hs-core/ langchain-hs-graph examples/

## Run Weeder dead code detection
weeder:
	weeder --config weeder.toml --hie-directory .hie --hie-directory langchain-hs-core/.hie --hie-directory langchain-hs-graph/.hie

## Build Haddock documentation
docs:
	stack haddock --no-haddock-deps langchain-hs-core langchain-hs-graph langchain-hs

## Build Hakyll documentation website
site-build:
	cd site && cabal run site -- build

## Run Hakyll preview server with live reload
site-watch:
	cd site && cabal run site -- watch

## Clean Hakyll build artifacts
site-clean:
	cd site && cabal run site -- clean

## Clean build artifacts
clean:
	stack clean
	cd site && cabal run site -- clean
