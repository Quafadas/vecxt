
You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

Here is the comparison of the standard `while` loop with the vectorised version.
<div id="vis" style="width: 50vw;height: 10vh"></div>

<script type="module">
  import vegaEmbed from "https://cdn.jsdelivr.net/npm/vega-embed@6/+esm?bundle-deps=true";
  var spec = "../../plots/sum.vg.json";
  vegaEmbed('#vis', spec).then(function(result) {
    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>

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