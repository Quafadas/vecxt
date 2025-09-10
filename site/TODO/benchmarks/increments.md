---
title: simple math operations
---

You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

## Finding the increments in an array


Here is the comparison of the standard `while` loop with the vectorised version.

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.incrementsBenchmark, node)
```


## Finding the variance of an array

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.varianceBenchmark, node)
```


# Conclusion

The case here is nuanced. The looped version is significantly faster, for small array sizes.

It could be, that the vectorised version is somehow inefficiently initiated. Whilst the case is more nuanced, I'm targeting larger data sizes, and so the vectorised version is left in, where it holds a cca 20% throughput advantage.