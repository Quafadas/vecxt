
You may need to refresh the page.
https://github.com/scala/scala3/issues/21637


In this benchmark, we check that repeatedly called matmul (i.e. doing the same as dgemm directly) is equivalent. This should demonstrate that the overhead of the function call is not significant - i.e. that there are no inappropriate allocations etc.

The benchmark code is [here](https://github.com/Quafadas/vecxt/blob/main/benchmark/src/matmul.scala)

And as long as the errors bars overlap, then these two implementations are equivalent.

<div id="vis" style="width: 50vw;height: 10vh"></div>

<script type="module">
  import vegaEmbed from "https://cdn.jsdelivr.net/npm/vega-embed@6/+esm?bundle-deps=true";
  var spec = "../../plots/matmul.vg.json";
  vegaEmbed('#vis', spec).then(function(result) {
    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>

So the benchmark boils down to a personal preference for which of these two pieces of code I'd rather write . They do exactly the same thing.

```scala mdoc:js sc:nocompile
import vecxt.facades._
showJsDocs("../../plots/matmul.vg.json", node)
```

```scala sc:nocompile
  val a : Matrix[Double] = ??? // some matrix
  val b : Matrix[Double] = ??? // some matrix

  //vecxt
  val multplied = a @@ b

  //blas
  val multiplied2 =
    blas.dgemm(
      "N",
      "N",
      a.rows,
      b.cols,
      a.cols,
      1.0,
      a.raw,
      a.rows,
      b.raw,
      b.rows,
      1.0,
      newArr,
      a.rows
    );
```
It is true, that some amount of flexibility is given up in terms of multiplying transposes etc. If that turns out to be painful, further extension methods could be considered.