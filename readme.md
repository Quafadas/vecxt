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

## Why?

Not?