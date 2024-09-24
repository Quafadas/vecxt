
You may need to refresh the page.
https://github.com/scala/scala3/issues/21637

Here is the comparison of the standard `while` loop with the vectorised version.
<div id="vis" style="width: 50vw;height: 10vh"></div>

<script type="module">
  import vegaEmbed from "https://cdn.jsdelivr.net/npm/vega-embed@6/+esm?bundle-deps=true";
  var spec = "../../plots/increments.vg.json";
  vegaEmbed('#vis', spec).then(function(result) {
    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>

# Conclusion

The case here is nuanced. The looped version is significantly faster, for small array sizes.

It could be, that the vectorised version is somehow inefficiently initiated. Whilst the case is more nuanced, I'm targeting larger data sizes, and so the vectorised version is left in.