## vecxt

Making cross platform, compute intense vector problems less... vexing.

Goals:

- Convienient syntax for vector operations
- Best "reasonably achievable", zero friction performance on each platform by delegating (inline) to a platform specific implementation - each platform is a first class citizen from a performance perspective.
- Extend scala std lib native and JS types - no new types or datastructures. The entire library is nothing more than a bunch of extension methods.

## Usage

Scala 3.3 +.

In scala-cli

```scala
//> using dep io.github.quafadas::vecxt::{{projectVersion}}
```
## What is it?

Aims to provide convienent and intuitive syntax for vector computations, whilst guaranteeing best-in-class performance on all platforms.



|    | JVM |JS  |Native|
|---|---|---|---|
| Data structure | `Array[Double]` | `Float64Array` | `Array[Double]` |
| Shims to | https://github.com/luhenry/netlib | https://github.com/stdlib-js/blas | [CBLAS](https://github.com/ekrich/sblas) |



## Didn't you say "cross platform"?

https://github.com/dragonfly-ai/narr

Surprise! Add that dependancy, express your computation in`Narray`... and you're now cross platform.

Note: Narr is not distributed "out the box", but it is recommended for anything cross platform.

## Syntax

Supported operationsa are currently basic (level 1 blas)

```scala
import vecxt.*
import narr.*
import vecxt.BoundsChecks.BoundsCheck.no

val v1 = NArray[Double](1.0, 2.0, 3.0)
val v2 = NArray[Double](3.0, 2.0, 1.0)

val v3 = v1 + v2

println(v3)

```