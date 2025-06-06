---
title:  Performance Perils
---

I decided that I wanted to find the sum of an Array[Double]. I also decided, that I was sensitive about the performance of it. So I broke out jmh, here are the results.

```
Benchmark                                               (matDim)   Mode  Cnt     Score     Error  Units
LinearAlgebraWorkloadBenchmark.stdlibSum                     500  thrpt    3  1067.563 ±  76.181  ops/s
LinearAlgebraWorkloadBenchmark.breezeMinimalTest             500  thrpt    3  4477.041 ± 669.565  ops/s
LinearAlgebraWorkloadBenchmark.vecxtMinimalTest              500  thrpt    3  8953.880 ± 733.304  ops/s
```

Here, the `stdlibSum` is the standard library's `Array[Double].sum`, which is a boxed operation, and thus slow. The `breezeMinimalTest` is using Breeze's optimized sum, and the `vecxtMinimalTest` is using a SIMD enhanced loop.

Using the the -prof stack and my friendly neighbourhood LLM, the conclusion is that stbLib is slow (described by Claude as "catastrophic boxing"). Breeze does a great job avoiding that and hitting a happy JVM path. Java's incubating SIMD API crushes even that.

The reason for posting this to the memory banks of my future self, is that everyone is more or less squatting on the `.sum` method name. It turns out, to be harder to call a definition like this;
```
extension (vec: Array[Double])
  inline def sum: Double =
    var i: Int = 0
    var acc = DoubleVector.zero(spd)

    while i < spd.loopBound(vec.length) do
      acc = acc.add(DoubleVector.fromArray(spd, vec, i))
      i += spdl
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

Than I would like - because scala's stdlib seems to gain priority. Although it's possible to call into it by specifiying out the package name,

`vecxt.arrays.sum(myVec)`

it's simply too natural to write
`myVec.sum`

For this convenience, you pay a cca 10x performance cost. As I'm not 100% clear how the `sum` method is resolved when you have more than one in scope. I concluded that it's hard to see what's happening unless you break out the profiler and zoom right in. I can't see a great way around this ambiguity. Operations at high risk of clashing, are therefore honoured with a weird SIMD suffix. The sensibly named counterparts remain and inline to them. Sadly, if you call the sensible named without a fully qualified package name, it's hard to know whether you're bleeding cca 8x performance or not.

```scala mdoc
import vecxt.all.*

val a = Array[Double](1, 2, 3, 6, 10, -100)

a.sumSIMD // SIMD optimized sum
a.productSIMD // SIMD optimized product
a.maxSIMD
a.minSIMD

```

I don't know how to implement SIMD on JS (or WASM) and native. On those platforms, these method names lie to you - they simply inline to a standard while loop.

With all this taken into account; here's the benchmark vs breeze;

```
Benchmark                                      (matDim)   Mode  Cnt     Score     Error  Units
LinearAlgebraWorkloadBenchmark.breezeWorkload       500  thrpt    3  2002.944 ± 284.520  ops/s
LinearAlgebraWorkloadBenchmark.vecxtWorkload        500  thrpt    3  2957.596 ± 424.765  ops/s
```

