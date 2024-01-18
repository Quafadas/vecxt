# vecxt

Making cross platform, compute intense vector problems less... vexing.

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

## JS installation

https://github.com/stdlib-js/blas

I'm not sure, what the right way of doing this is. In the absence of a bundler, it appears possible, to get it work for documenation purposes, by adding this script;

```
<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/stdlib-js/blas@umd/browser.js"></script>
```


With the following scalaJS facade, it remains unclear however, what the implication for bundlers is.
```
js.native
@JSImport("@stdlib/blas/base", JSImport.Default, "window.blas.base")
object blas extends BlasArrayOps
```

