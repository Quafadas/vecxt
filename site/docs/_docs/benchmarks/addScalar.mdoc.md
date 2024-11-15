
You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

Here is the comparison of the standard `while` loop with the vectorised version.

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.addScalarBenchmark, node)

```

And a comparison over time.

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.addScalarBenchmarkOverTime, node)

```


# Conclusion

To my surprise, the vectorised version is slower than the standard `while` loop, in nearly each case and across the two environments I have to test in. Although the volatility of the vectorised version is lower, the throughput is significantly lowe

It could be, that the JDK is simply very good at optimising this case. Feedback welcomed - but as the standard while loop outperforms the vectorised version, the standard `while loop` version is left included.