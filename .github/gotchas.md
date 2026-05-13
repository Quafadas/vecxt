
## Gotchas

### JMH `Unit`-returning benchmarks and the Vector API scalar-replacement cliff

**Symptom**: A JMH benchmark that calls SIMD code (Vector API) shows a sudden, dramatic throughput cliff (100–500×) at a specific array size threshold, accompanied by massively inflated GC allocation (`gc.alloc.rate.norm` jumping from the expected one-array-worth to 30–70× that, e.g. 65 KB/op becomes 2.4 MB/op). The warmup iterations look healthy; the compiled measurement iterations are catastrophically slow. The compiled code is *slower* than the interpreter.

**Root cause**: C2 fails to scalar-replace `FloatVector`/`VectorMask` objects when the enclosing JMH benchmark method has a `Unit` return type (`def bench(bh: Blackhole): Unit`). The JIT sees the `bh` reference pre-loaded at the bottom of the operand stack and its escape analysis incorrectly concludes the transient Vector objects escape. They are heap-allocated every iteration, causing cascading GC pressure.

**The library code is not broken.** The same SIMD path invoked from a non-void return method measures correctly (expected alloc, linear scaling). Do not spend time changing `logicalFloatIdx`, `spf` declarations, loop structure, or any library code in response to this symptom — it is a benchmark artefact.

**Fix**: Change the benchmark method to return its result explicitly:
```scala
// BAD — triggers the cliff
@Benchmark
def my_op(bh: Blackhole): Unit =
  bh.consume(arr > 0.0f)

// GOOD — C2 scalar-replaces Vector objects correctly
@Benchmark
def my_op(bh: Blackhole): Array[Boolean] =
  val result = arr > 0.0f
  bh.consume(result)
  result
```

**When diagnosing a performance regression**:
1. First check `gc.alloc.rate.norm` with `-prof gc`. Expected alloc is `sizeof(output array) + small constant`.
2. If alloc is 30–70× too high, suspect this JMH anti-pattern before touching library code.
3. Confirm by adding a `_returning` variant that returns the result — if it measures correctly, the benchmark method signature is the culprit.

**Not affected**: Benchmarks that mutate in-place (`Unit` is fine), or those where `bh.consume` is called on a pre-existing field/variable that was allocated outside the hot loop.