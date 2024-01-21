# Vecxt

Making cross plaform vector problem less... vexing

```scala
import vecxt._
import BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

println((v1 + v2).mkString("[",",","]"))

```

## Goals

- Inline calls to platform native BLAS implementations for maximum performance
- Provide a common interface to vector operations across platforms
- Intuitive sytax for those used to working with linear algebra libraries
- No custom data-structures - the entire library is extension methods on `Array[Double]`

## Non-Goals

- General purpose linear algebra - see [slash](https://github.com/dragonfly-ai/slash) or [breeze](https://github.com/scalanlp/breeze/)
- Visualisation - see [dedav4s](https://quafadas.github.io/dedav4s/)
- Being much more than a shim to platform native BLAS implementations

## Status

At the time of writing, it's fairly incomplete, but the concept works and solves my own (simple, niche) needs in a simple way.

Free to open a discussion or PR if you'd want something else from it.