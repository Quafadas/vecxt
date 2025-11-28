# Benchmarking


# Benchmarking

This project, was my first foray into something that is performance sensitive. All the benchmarks we publish here, are run in CI. We will be using JMH (for the JVM) and mills JMH plugin. I investigated this action for CI;

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

Now we're in a position to plot some benchmarks. The plotting mechanism is a little experimental. We serialize named tuples into vega specs. This allows us to build "the same" spec out of parts, and lightly customise them. The plots themselves reference the data that is continuously updated out of the CI.

Finally, we use mdoc JS to provide the div, and then the hook to actually run the plots.

# Conlusion

We have built a pipline which ensures that we can ensure this library maintains compelling performance characteristics, by measuring, collecting the data, and plotting the data to prove it.



