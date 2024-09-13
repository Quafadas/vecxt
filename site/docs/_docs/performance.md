# Performance

The core aim of this library, is to provide platform native, friction free performance. I'm confident that I can't do better than this - at least outside of an absurd about of effort.

In general cross platform performance is a hard problem. We sidestep it by simply providing compiletime `@inline` shims to platform native BLAS implementations.


||JVM|JS|Native|Cross|
----|----|----|----|---|
Data structure| `Array[Double]` | `Float64Array` | `Array[Double]` |`NArray[Double]` |
Shims to | https://github.com/luhenry/netlib | https://github.com/stdlib-js/blas | [CBLAS](https://github.com/ekrich/sblas) | Best available |

Consider browsing the [[vecxt]] api, and particulaly the extensions object. You'll see that most definitions are `@inline` anotated - i.e. there is zero runtime overhead calling this library.

### JS

On Node, this shim ships with it's own C BLAS implementation.

In browser / elsewhere, it's done in loop-unrolled native arrays.

TODO: Investigate webassembly? 