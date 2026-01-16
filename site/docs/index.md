# Vecxt

Making cross plaform vector problem less... vexing

```sh
scala-cli repl --dep io.github.quafadas::vecxt::@VERSION@ --java-opt "--add-modules=jdk.incubator.vector" --repl-init-script 'import vecxt.all.{*, given}'
```

Getting started with scala cli

```scala
//> using dep io.github.quafadas::vecxt::@VERSION@

// If you're on the JVM
//> using javaOpt "--add-modules=jdk.incubator.vector"
```

### Mill
```scala sc:nocompile
ivy"io.github.quafadas::vecxt::@VERSION@"
```

```scala mdoc
import vecxt.all.{*, given}

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

v1 + v2

val v3 = Array.fill(3)(0.0)
v3 -= v2
v3


```

## Goals

- Where possible inline calls to platform-native-BLAS implementations for maximum performance\
- Zero copy semantics / views on contiguous unbroken arrays of data for performance
- Reasonable, consistent cross platform ergonomics
- Very little / no data-structures - the vector part of the library is an extension method on `Array[Double]` for example
- A single cross platform test suite
- Simplicity, speed

## Non-Goals

- General mathematics - lets' try to keep a focus on linear algebra. See [slash](https://github.com/dragonfly-ai/slash) or [breeze](https://github.com/scalanlp/breeze/) for more general libraries
- Visualisation - see [dedav4s](https://quafadas.github.io/dedav4s/)