
You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

Here is the comparison of the standard `while` loop with the vectorised version, when counting the true values in a boolean array.

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.countTrueBenchmark, node)

```

Doing an "and" operation

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.andBooleanBenchmark, node)

```

# Conclusion
Boolean processing gains massive benefits from SIMD.
