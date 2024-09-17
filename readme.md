# vecxt

Making cross platform, compute intense vector problems less... vexing.

Vecxt aims to free you from the Architectural tyranny of having to choose which platform you run your

## What is it?

Aims to provide convienent and intuitive syntax for vector computations, whilst guaranteeing best-in-class performance on all platforms.

||JVM|JS|Native|
----|----|----|----|
Data structure| `Array[Double]` | `Float64Array` | `Array[Double]` |
Shims to | https://github.com/luhenry/netlib | https://github.com/stdlib-js/blas | [CBLAS](https://github.com/ekrich/sblas) |

## Didn't you say "cross platform"?

https://github.com/dragonfly-ai/narr

Surprise! Add that dependancy, express your computation in `NArray` and get best-in-class, native performance (no runtime overhead) on all platforms.

Note: Narr is not distributed "out the box", but it is recommended for anything cross platform.

## Syntax

Supported operationsa re currently very basic, but we share a single test suite across all platforms, e.g.

```scala

  val v1 = NArray[Double](1.0, 2.0, 3.0)
  val v2 = NArray[Double](3.0, 2.0, 1.0)

  val v3 = v1 + v2

  assertEqualsDouble(v3(0), 4, 0.00001)
  assertEqualsDouble(v3(1), 4, 0.00001)
  assertEqualsDouble(v3(2), 4, 0.00001)

```

## Why?

Not?


