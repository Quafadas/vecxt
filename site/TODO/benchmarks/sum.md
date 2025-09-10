---
title: Sum
---

You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

Here is the comparison of the standard `while` loop with the vectorised version for doubles.

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.sumBenchmark, node)
```

And ints;

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*
showJsDocs.fromSpec(BenchmarkPlots.sumIntBenchmark, node)
```


And the function left in vexct over time (against regressions)

```scala mdoc:js sc:nocompile
import vecxt.plot.*
import vecxt.facades.*

showJsDocs.fromSpec(BenchmarkPlots.sumBenchmarkOverTime, node)
```

The two implementations are;

```scala sc:nocompile
extension (vec: Array[Double])

   inline def sum_loop =
      var sum: Double = 0.0
      var i: Int = 0
      while i < vec.length do
        sum = sum + vec(i)
        i = i + 1
      end while
      sum
    end sum3


  inline def sum_vec: Double =
      var i: Int = 0

      var acc = DoubleVector.zero(Matrix.doubleSpecies)
      val sp = Matrix.doubleSpecies
      val l = sp.length()

      while i < sp.loopBound(vec.length) do
        acc = acc.add(DoubleVector.fromArray(Matrix.doubleSpecies, vec, i))
        i += l
      end while
      var temp = acc.reduceLanes(VectorOperators.ADD)
      // var temp = 0.0
      while i < vec.length do
        temp += vec(i)
        i += 1
      end while
      temp
    end sum

```

# Conclusion

For small array sizes (n = 3) the implementatiosn are the same. This would be expected, as the arrays are too small to vectorise. The fact the means are so close, gives us some confidence, that the measurements are realistic.

There does appear to be significant advantage to the vectorised version (2-4x) depending on array size (100 - 100,000). I'm surprised as I was expecting the JVM to almost perfectly optimised on the `while` loop. Perhaps, it is not able to easily auto-vectorise scala. I would welcome opinon.