---
title: Benchmarks
---

Please note :

Viewing the plots _will not work_ unless you navigate there directly or press the refresh button in browser. I believe this is a scaladoc bug.

https://github.com/scala/scala3/issues/21637

All benchmarks are run in a single pass on whatever is assigned to the GHA benchmark job.

The benchmarking parameters are copied from the BLAS [benchmarking suite](https://github.com/luhenry/netlib/tree/master/benchmarks/src/main/java/dev/ludovic/netlib/benchmarks/blas) here. It is assumed, that the BLAS setup is a reasonable way to test our own.

As this is my first foray into benchmarking, feedback is welcomed.