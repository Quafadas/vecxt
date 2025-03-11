# Vecxt Style Guide

This style guide outlines the conventions and best practices for contributing to the Vecxt project. Following these guidelines helps maintain consistency and readability across the codebase.

## General Guidelines

- **Use inline methods**: Inline methods are preferred for performance reasons.
- **Use Scala's `tap` method**: Use `tap` for chaining operations on collections and arrays for readability.
- **Use `while` loops**: Prefer `while` loops over `for` loops for performance-critical sections, especially in JS and Native targets.
- **Use Java's SIMD `Vector` API**: On the JVM, leverage Java's SIMD `Vector` API for vectorized operations.
- **Use tail recursion**: When implementing recursive methods, prefer tail recursion for performance improvements.
- **Avoid side effects**: Try to minimize or eliminate side effects in methods to enhance clarity and maintainability.

## Code Structure

- **Package Structure**: Organize code into appropriate packages. For example, linear algebra operations should be in the `vecxt` package.
- **Imports**: Group imports by standard library, third-party libraries, and project-specific imports. Use wildcard imports for project-specific packages.
- **Tests**: Use scala munit and are found in the 'test' subdirectory. Cross platform tests should be in 'test/src'

## Naming Conventions

- **Classes and Objects**: Use PascalCase for class and object names.
- **Methods and Variables**: Use camelCase for method and variable names.
- **Constants**: Use UPPER_SNAKE_CASE for constants.

## Documentation

- **Comments**: Use comments to explain complex logic and provide context. Avoid obvious comments.
- **Docstrings**: Use Scaladoc comments (`/** ... */`) for public methods and classes. Include descriptions, parameters, and return values.

## Scala Specifics

- **Use `val` over `var`**: Prefer immutable values (`val`) over mutable variables (`var`).
- **Pattern Matching**: Use pattern matching for conditional logic instead of chained `if-else` statements.
- **Option Handling**: Use `Option` and pattern matching to handle nullable values.

## Performance

- **Vectorized Operations**: Use vectorized operations with Java's SIMD `Vector` API on the JVM.
- **Bounds Checking**: Use the `BoundsCheck` typeclass to handle bounds checking in array operations.
- **Avoid Unnecessary Allocations**: Minimize unnecessary allocations, especially in performance-critical sections.

## Example Code

Here's an example of a well-structured method:

```scala
package vecxt

import jdk.incubator.vector.DoubleVector
import vecxt.BoundsCheck.BoundsCheck

object arrays:

  extension (vec: Array[Double])

    inline def +(d: Double): Array[Double] =
      val out = new Array[Double](vec.length)
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .add(inc)
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) + d
        i = i + 1
      end while
      out
    end +