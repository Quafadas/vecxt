# Vecxt: Cross platform vector and matiricies on scala 3

ALWAYS reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

This is a scala 3 project using the mill build tool. It is a lightweight, cross platform (JVM, JS, Native), performant linear algebra library.

On the JVM, use javas (currently incubating) SIMD 'Vector' API where possible. On JS and native, use while loops.

Answer all questions in the style of a friendly colleague that is an expert in linear algebra and computational mathematics.

## Working Effectively

Mill may be found via it's wrapper script `./mill` in the root of the repository. For example `./mill vecxt.__.compile` will compile the JVM, JS and native targets.

If you are on windows, use `mill`, not `./mill`.

Each module contains it's own build definition in the package.mill file in it's module directory.

- BUILDS: Cold compilation may take 1 minute or so the _first_ time any compilation is run. Subsequent (incremental) compilations are fast.
- Compile specific platforms (e.g. jvm) with `./mill vecxt.jvm.compile` or `./mill vecxt.js.compile` etc.
- Run all tests by following with the same pattern `./mill vecxt.__.test`
- Format code with `./mill mill.scalalib.scalafmt.ScalafmtModule/`. CI will enforce formatting, and will fail if code is not formatted.
- If you see an error like this is JS `[error] @scala.scalajs.js.annotation.internal.JSType is for compiler internal use only. Do not use it yourself.`, run `./mill clean vecxt.js._` to clear the build cache.
- To run a specific main class, use the runMain command and specify the package. `./mill experiments.runMain testCheatsheet` for example.

## Folder structure

vecxt/
├── .github/                    # GitHub workflows and copilot instructions
│   └── copilot-instructions.md # Developer guidance for AI assistance
├── .devcontainer/             # VS Code dev container configuration
├── .vscode/                   # VS Code workspace settings
├── build.mill                   # Mill build configuration (main build file)
├── mill                      # Mill wrapper script for cross-platform builds
├── styleguide.md             # Coding style guidelines
├── benchmarks/              # Benchmarking code - not published, may be run in CI on request
├── experiments/              # Not published, inlined experiments - use this as a sandbox
├── vecxtensions/             # Published experimental / concepts which may not be suitable for main module
│   ├── src/                  # Cross-platform shared source code
│   ├── src-jvm/              # JVM-specific implementations (SIMD Vector API)
│   ├── src-js/               # JavaScript-specific implementations
│   ├── src-native/           # Scala Native-specific implementations
│   └── test/                 # Cross-platform test suite (munit)
│       ├── src/              # Shared test source files
│       ├── src-jvm/          # JVM-specific tests
│       ├── src-js/           # Js-specific tests
│       └── src-native/       # Scala Native-specific tests
├── vecxt_re/                 # Domain specific library for reinsurance calculations
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
Use inline methods where possible to avoid dispatch overhead.

## GitHub Actions CI
The project uses GitHub Actions for CI/CD

## Gotchas

### JMH `Unit`-returning benchmarks and the Vector API scalar-replacement cliff

**Symptom**: A JMH benchmark that calls SIMD code (Vector API) shows a sudden, dramatic throughput cliff (100–500×) at a specific array size threshold, accompanied by massively inflated GC allocation (`gc.alloc.rate.norm` jumping from the expected one-array-worth to 30–70× that, e.g. 65 KB/op becomes 2.4 MB/op). The warmup iterations look healthy; the compiled measurement iterations are catastrophically slow. The compiled code is *slower* than the interpreter.

**Root cause**: C2 fails to scalar-replace `FloatVector`/`VectorMask` objects when the enclosing JMH benchmark method has a `Unit` return type (`def bench(bh: Blackhole): Unit`). The JIT sees the `bh` reference pre-loaded at the bottom of the operand stack and its escape analysis incorrectly concludes the transient Vector objects escape. They are heap-allocated every iteration, causing cascading GC pressure.

**The library code is not broken.** The same SIMD path invoked from a non-void return method measures correctly (expected alloc, linear scaling). Do not spend time changing `logicalFloatIdx`, `spf` declarations, loop structure, or any library code in response to this symptom — it is a benchmark artefact.

**Fix**: Change the benchmark method to return its result explicitly:
```scala
// BAD — triggers the cliff
@Benchmark
def my_op(bh: Blackhole): Unit =
  bh.consume(arr > 0.0f)

// GOOD — C2 scalar-replaces Vector objects correctly
@Benchmark
def my_op(bh: Blackhole): Array[Boolean] =
  val result = arr > 0.0f
  bh.consume(result)
  result
```

**When diagnosing a performance regression**:
1. First check `gc.alloc.rate.norm` with `-prof gc`. Expected alloc is `sizeof(output array) + small constant`.
2. If alloc is 30–70× too high, suspect this JMH anti-pattern before touching library code.
3. Confirm by adding a `_returning` variant that returns the result — if it measures correctly, the benchmark method signature is the culprit.

**Not affected**: Benchmarks that mutate in-place (`Unit` is fine), or those where `bh.consume` is called on a pre-existing field/variable that was allocated outside the hot loop.

## Vecxt Re

Contains a bunch of domain specific code for reinsurance calculations, structures, and various reinsurance contract types. It will often rely on Vecxt. You should view the principles as the same - correctness above all else - performance matters. It also aims to eexpose a consistent cross platform API.