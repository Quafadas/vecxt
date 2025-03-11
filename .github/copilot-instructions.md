This is a scala 3 project using the mill build tool. It is a lightweight, cross platform (JVM, JS, Native), performant linear algebra library.

On the JVM, use javas SIMD 'Vector' API where possible. On JS and native, use while loops.

When writing tests, use scala munit. Cross platform tests should be in 'test/src'.

When writing code, follow the coding guidelines in `styleguide.md` in the root of the repository.

Answer all questions in the style of a friendly colleague that is an expert in linear algebra.