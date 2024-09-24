
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


```scala sc:nocompile
extension (vec: Array[Double])

  @Benchmark
  def vecxt_mmult(bh: Blackhole)=
    val cclone = matA @@ matB
    bh.consume(cclone);
  end vecxt_mmult

  @Benchmark
  def java_dgemm(bh: Blackhole) =
    val cclone = Array.fill[Double](m*n)(0)
    blas.dgemm(
      transa,
      transb,
      m,
      n,
      k,
      alpha,
      a,
      if transa.equals("N") then m else k,
      b,
      if transb.equals("N") then k else n,
      beta,
      cclone,
      m
    );
    bh.consume(cclone);
  end java_dgemm


```