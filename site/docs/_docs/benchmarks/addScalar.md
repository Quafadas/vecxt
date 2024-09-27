
You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

Here is the comparison of the standard `while` loop with the vectorised version.
<div id="vis" style="width: 50vw;height: 10vh"></div>

And a comparison over time.

<div id="visTime" style="width: 50vw;height: 10vh"></div>

<script type="module">
  import vegaEmbed from "https://cdn.jsdelivr.net/npm/vega-embed@6/+esm?bundle-deps=true";
  var spec = "../../plots/addScalar.vg.json";
  vegaEmbed('#vis', spec).then(function(result) {
    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>

And over time

<script type="module">
  import vegaEmbed from "https://cdn.jsdelivr.net/npm/vega-embed@6/+esm";
  var spec = "../../plots/addScalar_over_time.vg.json";
  vegaEmbed('#visTime', spec)
</script>


# Conclusion

To my surprise, the vectorised version is slower than the standard `while` loop, in nearly each case and across the two environments I have to test in.

It could be, that the JDK is simply very good at optimising this case. Feedback welcomed - but as the standard while loop outperforms the vectorised version, the standard `while loop` version is left included.