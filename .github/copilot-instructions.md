# Vecxt: Cross platform vector and matiricies on scala 3

ALWAYS reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

This is a scala 3 project using the mill build tool. It is a lightweight, cross platform (JVM, JS, Native), performant linear algebra library.

On the JVM, use javas (currently incubating) SIMD 'Vector' API where possible. On JS and native, use while loops.

Answer all questions in the style of a friendly colleague that is an expert in linear algebra and computational mathematics.

## Working Effectively

Mill may be found via it's wrapper script `./millw` in the root of the repository. For example `./millw vecxt.__.compile` will compile the JVM, JS and native targets.

Each module contains it's own build definition in the package.mill file in it's module directory.

- BUILDS: Mill cold compilation takes 2 minutes or so. Tests take 1-3 minutes from cold. Set timeout to 2+ minutes.
- Compile specific platforms (e.g. jvm) with `./millw vecxt.jvm.compile` or `./millw vecxt.js.compile` etc.
- Run tests with the same patterns `./millw vecxt.__.test`
- Format code with `./millw mill.scalalib.scalafmt.ScalafmtModule/`. CI will enforce formatting, and will fail if code is not formatted.
- If you see an error like this is JS `[error] @scala.scalajs.js.annotation.internal.JSType is for compiler internal use only. Do not use it yourself.`, run `./millw clean vecxt.js._` to clear the build cache.

## Folder structure

vecxt/
├── .github/                    # GitHub workflows and copilot instructions
│   └── copilot-instructions.md # Developer guidance for AI assistance
├── .devcontainer/             # VS Code dev container configuration
├── .vscode/                   # VS Code workspace settings
├── build.mill                   # Mill build configuration (main build file)
├── millw                      # Mill wrapper script for cross-platform builds
├── styleguide.md             # Coding style guidelines
├── benchmarks/              # Benchmarking code - not published, may be run in CI on request
├── experiments/              # Not published, inlined experiments - use this as a sandbox
├── vecxtensions/             # Published module with experiments / concepts which may not be suitable for main module
│   ├── src/                  # Cross-platform shared source code
│   ├── src-jvm/              # JVM-specific implementations (SIMD Vector API)
│   ├── src-js/               # JavaScript-specific implementations
│   ├── src-native/           # Scala Native-specific implementations
│   └── test/                 # Cross-platform test suite (munit)
│       ├── src/              # Shared test source files
│       ├── src-jvm/          # JVM-specific tests
│       ├── src-js/           # Js-specific tests
│       └── src-native/       # Scala Native-specific tests
├── vecxt/                    # Main source directory and core published module
│   ├── src/                  # Cross-platform shared source code
│   ├── src-jvm/              # JVM-specific implementations (SIMD Vector API)
│   ├── src-js/               # JavaScript-specific implementations
│   ├── src-js-native/        # JavaScript / native shared (DRY) implementations
│   ├── src-native/           # Scala Native-specific implementations
│   └── test/                 # Cross-platform test suite (munit)
│       ├── src/              # Shared test source files
│       ├── src-jvm/          # JVM-specific tests
│       ├── src-js/           # Js-specific tests
│       └── src-native/       # Scala Native-specific tests
├── site/                    # Source for docsite
│   └── docs/                  # Markdown files for docsite
└── README.md

##  Validation

The primary form of validation is via unit testing.

Tests are writting using scala munit, see the folder structure for locations.

In general it is expected that all tests, pass on all platforms. The number 1 goal of this library is *correctness* - this goal takes precedeance above all else. Continually check and flag potentially any suspected incorrect calculation.

The second, equally important goal of this library is *performance* and *usability*. Prefer usability where forced to choose against performance.

The third goal of this library is *cross platform consistency*.

Cross platform consistency is measured by compliance with the cross platform tests.

When writing code, follow the coding guidelines in `styleguide.md` in the root of the repository.

## Code Guidelines
Follow styleguide.md for coding conventions
Use inline methods where possible to avoid dispatch overhead where possible.

## GitHub Actions CI
The project uses GitHub Actions for CI/CD