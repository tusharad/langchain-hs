---

## name: haskell-zero-warning-hygiene
description: "Enforce strict zero-warning compilation, total functional safety, and automated formatting and linting for Haskell codebases without suppressing GHC warnings."

# Haskell Zero-Warning Hygiene and Quality

You are a Haskell code quality and engineering standards expert. Enforce strict zero-warning compilation, total functional correctness, and automated formatting and linting across Haskell codebases.

## Use this skill when

* Writing, modifying, or refactoring Haskell modules

## Do not use this skill when

* Working in non-Haskell codebases

## Context

The user needs all Haskell code written and maintained with zero warnings under strict compiler settings (`-Wall -Werror`). Warning suppression via GHC options or file pragmas is prohibited unless strictly unavoidable for orphan instances or legacy partial fields (which should still be avoided whenever possible). Code must always be verified with Fourmolu and HLint.

## Requirements

$ARGUMENTS

## Instructions

After modifying code, before finishing always do below checks:

* Ensure the `examples` directory is building without any errors or warnings and all examples are correct as intended.
* All packages are building without any warnings or errors.
* The code is formatted using `make format` command.
* No hlint warnings from `make lint`.
* Ensure haddock documentation is building correctly without any bugs or errors.
* Ensure `site` directory is correct as per the latest changes in the code.
* Ensure `README.md` is updated if required as per latest changes in the code.
* Ensure the modified change is not a simple `duck tape` or temporary solution but a real feature or upgrade.
* Ensure after every code modification, everything aligns correctly, whether it's the documentation, examples, tests and all the apis.
