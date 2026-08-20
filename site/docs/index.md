# Vecxt

Making cross plaform vector problem less... vexing

```sh
scala-cli repl --dep io.github.quafadas::vecxt::@VERSION@ --java-opt "--add-modules=jdk.incubator.vector" --repl-init-script 'import vecxt.all.*'
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
### Intro

The obvious [vector operations](vectors-and-matrices/examples.md).

```scala mdoc
import vecxt.all.*

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

v1 + v2

val v3 = Array.fill(3)(0.0)
v3 -= v2
v3
```
The core of `vecxt` is nothing more than a bunch of extension methods on `Array[Double]`, `Array[Float]` etc... design stupid... but attractively simple.

Matricies look like this.

```scala mdoc
import vecxt.all.*

val m1 = Matrix.fromRows[Double](
    Array(1.0, 2.0, 3.0),
    Array(4.0, 5.0, 6.0),
    Array(7.0, 8.0, 9.0)
)

println(m1.printMat)

val t1 = m1.transpose // zero copy
println(t1.printMat)
val slice1 = m1(1 to 2, 1 to 2) // zero copy

slice1.shape

val matmul = (1.0 +  m1) @@ t1( :: , 0 to 1) 
// 1.0 + m1 -> SIMD accelerated 
// t(::, 0 to 1) ->  slice of a slice, still zero copy
// m @@ m -> BLAS accelerated

println(matmul.shape)
println(matmul.printMat)

```

NDArray

```scala

```

## Goals

- Pythonic syntax
- Where possible inline calls to platform-native-BLAS implementations for maximum performance\
- Zero copy semantics / views on contiguous unbroken arrays of data for performance
- Reasonable, consistent cross platform ergonomics
- Very few custom data-structures - the vector part of the library is an extension method on `Array[Double]` for example
- A single cross platform test suite
- Simplicity, speed

## Non-Goals

- Visualisation - see [dedav4s](https://quafadas.github.io/dedav4s/)
- Data ingestion - see [scautable](https://github.com/Quafadas/scautable)