---
title: Benchmarking
---

# Benchmarking

This project, was my first foray into something that is performance sensitive. We will be using JMH (for the JVM) and mills JMH plugin. I investigated this action for CI;

https://github.com/benchmark-action/github-action-benchmark

But ended unconvinced that it was the right thing. It uses the older style github pages from branch deployment. I also didn't understand what happened to it's outputs, and how I could hook in myself outside of the plot itself. There was a bewildering array of knobs and levers which I failed to tweak to my usecase.

From first principles, I thought about 3 problems;
- How should a benchmark be triggered?
- Where shoudl the data be stored
- How to consume

## Trigger

I concluded I wanted a manual trigger. Each push to main is too much. Benchmareking is by definition compute intense, and doing it continuously is rather wasteful outside of something truly mission critical. In future perhaps switch to benchmark on  release. Currently, manual makes more sense.

## Where should the data be stored

JMH itself doesn't give you more than the results. We need extra metadata. ChatGPT wrote me a shell script which appends the date, commit and branch that the results were generated from into the results html. This will be our metadata.

This file is then pushed to an orphaned  [branch](https://github.com/Quafadas/vecxt/tree/benchmark). The benchmarking process ends here - it only stores the data, it's then left to the consumer to figure out what to do with it.

## Consumption

During the github pages build step (i.e. in GHA) This file will be added to the static assets of the site, we switch into our orphan branch in the github action and aggregate the results into a single benchmarking.json file, we further post process data data from the step above to flatten all results into a single array.

It may be found [here](../../benchmarks/benchmark_history.json);

Now we're in a position to plot some benchmarks.

```javascript
<div id="vis" style="width: 50vw;height: 10vh"></div>

<script type="module">
  import vegaEmbed from "https://cdn.jsdelivr.net/npm/vega-embed@6/+esm?bundle-deps=true";
  var spec = "../../plots/addScalar.vg.json";
  vegaEmbed('#vis', spec).then(function(result) {
    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>
```

This javascript embeds a vega plot into the page. Now, we need only to provide a plotfile per visualisation. Ideally, we would abstract over the visualisations we need - my pet project dedav4s would be great for this, but scalaJS in mdoc is currently a challenge.



