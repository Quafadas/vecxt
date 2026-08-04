package vecxt.jit

import munit.FunSuite
import vecxt.all.{*, given}

/** D1 — Allocation-free kernels.
  *
  * Per {@code @AllocFree} kernel: assert that the steady-state bytes/op returned by {@link AllocMeter#measureAlloc} is
  * at most {@code Eps}. {@link AllocMeter} uses a slope approach — {@code max(0, alloc_2N − alloc_N)} — so a bounded
  * JIT-compilation burst that lands in the first measurement window but not the second is cancelled by the subtraction,
  * and the callers see zero.
  *
  * Epsilon = 8 bytes/op rationale: for a correctly JIT-compiled kernel the slope is exactly zero. The epsilon provides
  * headroom equal to one object header, accounting for any residual TLAB-accounting noise. A {@code DoubleVector}
  * failing to scalarise would be ≥ 64 bytes in the raw window; after burst-cancellation the slope for a single
  * unscalarised vector per call is roughly {@code 64 − burst/reps ≈ 49 bytes/op} on CI (with the observed burst of ~1.5
  * MB over 100 000 iterations), still comfortably above the 8-byte threshold.
  *
  * Tests that return a value (the reductions — {@code sumSIMD}, {@code productSIMD}, {@code dot}, {@code minSIMD},
  * {@code maxSIMD}, {@code variance}) store their result into a {@code @volatile} field so that C2 cannot
  * dead-code-eliminate the computation. Without a sink, a kernel whose result is discarded is pure (no observable side
  * effects) and C2 is free to eliminate the entire loop body, giving zero measured allocation not because the vectors
  * were scalarized but because nothing ran. The in-place mutations ({@code +=}, {@code -=}, {@code abs!}, etc.) write
  * to an array, which is a visible side effect, so they are not exposed to this hazard.
  *
  * <p>Coverage is deliberately every method carrying {@code @AllocFree}, and the two are kept in step by hand: an
  * annotation with no test here is an assertion nobody has checked, which is how {@code intarrays.dot} carried
  * {@code @AllocFree} through two releases while allocating a dead array per call, and how {@code **!} kept it until
  * #110 measured it. The list is written out rather than driven from a collection on purpose — see the note on
  * {@code assertAllocFree} below.
  *
  * {@code assertAllocFree} is declared {@code inline} so that each call site gets its own specialised measurement loop
  * inside {@code AllocMeter.measureAlloc}. Without inlining the body would be dispatched through a shared megamorphic
  * {@code Function0.apply()} call that C2 would not inline through, preventing SIMD intrinsification.
  *
  * Tests are skipped (not failed) when {@code ThreadMXBean} allocation tracking is not supported and we are not on CI.
  * On CI (environment variable {@code CI} is set) a missing bean is a hard failure, because a guardrail that silently
  * becomes a no-op is indistinguishable from a passing guardrail.
  */
class D1Suite extends FunSuite:

  private val N = 1024
  private val Eps = 8L // bytes/op; see class doc
  private val Warmup = 20_000
  private val Reps = 100_000

  // Volatile sinks for pure-reduction results. Storing into a @volatile field is an observable
  // side effect that prevents C2 from dead-code-eliminating the reduction body. Primitive-typed
  // fields avoid boxing (no allocation per store).
  @volatile private var doubleSink: Double = 0.0
  @volatile private var floatSink: Float = 0.0f
  @volatile private var intSink: Int = 0

  private inline def assertAllocFree(label: String)(inline body: => Unit): Unit =
    val total = AllocMeter.measureAlloc(Warmup, Reps)(body)
    // Hard-fail in CI: a missing bean means the guardrail has silently become a no-op.
    if total < 0L && sys.env.contains("CI") then
      fail(
        s"[D1] $label: CI detected but ThreadMXBean allocation tracking is unavailable. " +
          "The pinned runner (Temurin 25) must support per-thread allocation tracking. " +
          "Check JVM configuration or the runner image."
      )
    end if
    assume(total >= 0L, s"[D1] skip $label — ThreadMXBean allocation tracking not available on this JVM")
    val perOp = total.toDouble / Reps
    assert(
      perOp <= Eps,
      s"D1 github.com/Quafadas/vecxt/issues/105: $label allocated ${perOp.toLong} bytes/op " +
        s"(total $total bytes over $Reps iterations). " +
        s"A DoubleVector failing to scalarize is ≥ 64 bytes/op. " +
        s"Check that jdk.incubator.vector is on the module path and that " +
        s"the species field is static final, not a method parameter."
    )
  end assertAllocFree

  // ── Double ──────────────────────────────────────────────────────────────────

  test("D1: doublearrays.sumSIMD") {
    val arr = Array.tabulate(N)(_.toDouble)
    assertAllocFree("doublearrays.sumSIMD") { doubleSink = arr.sumSIMD }
  }

  test("D1: doublearrays.productSIMD") {
    // Values close to 1.0 to avoid overflow/underflow across many calls.
    val arr = Array.fill(N)(1.0001)
    assertAllocFree("doublearrays.productSIMD") { doubleSink = arr.productSIMD }
  }

  test("D1: doublearrays.+=(Double)") {
    val arr = Array.fill(N)(1.0)
    assertAllocFree("doublearrays.+=(Double)")(arr += 0.1)
  }

  test("D1: doublearrays.-=(Double)") {
    val arr = Array.fill(N)(5.0)
    assertAllocFree("doublearrays.-=(Double)")(arr -= 0.1)
  }

  test("D1: doublearrays.*=(Array[Double])") {
    val arr = Array.fill(N)(2.0)
    // Multiplier of 1.0 keeps the array values stable across 120,000 iterations.
    // Using 0.5 would drive values to denormals by iteration ~1,100 and then to zero,
    // so the measured workload would be overwhelmingly operating on zeros.
    val arr2 = Array.fill(N)(1.0)
    assertAllocFree("doublearrays.*=(Array[Double])")(arr *= arr2)
  }

  test("D1: doublearrays.fma!") {
    val arr = Array.fill(N)(1.0)
    assertAllocFree("doublearrays.fma!")(arr.`fma!`(2.0, 0.5))
  }

  test("D1: doublearrays.clamp!") {
    val arr = Array.tabulate(N)(i => (i % 10).toDouble)
    assertAllocFree("doublearrays.clamp!")(arr.`clamp!`(2.0, 7.0))
  }

  test("D1: doublearrays.fillLinspace") {
    val dest = new Array[Double](N)
    assertAllocFree("doublearrays.fillLinspace")(fillLinspace(dest, 0.0, 1.0))
  }

  // The two in-place unary kernels that carry @AllocFree. NEG and ABS are intrinsified
  // lanewise operations, so the DoubleVector temporaries and the masked-tail VectorMask are
  // expected to scalarize. The transcendental members of the same family (exp!, log!, sin!, …)
  // are deliberately *not* annotated and not measured here — #110 already found `**!` allocating
  // for what is most likely the same reason, and asserting zero for an operation that falls back
  // to a software path would be asserting a hope.
  test("D1: doublearrays.-!") {
    val arr = Array.fill(N)(1.0)
    assertAllocFree("doublearrays.-!")(arr.`-!`)
  }

  test("D1: doublearrays.abs!") {
    val arr = Array.tabulate(N)(i => if i % 2 == 0 then i.toDouble else -i.toDouble)
    assertAllocFree("doublearrays.abs!")(arr.`abs!`)
  }

  // `variance(mode)` reads one field out of the `MeanAndVariance` that `meanAndVarianceTwoPass` returns and discards
  // the other, so the result object is dead and escape analysis scalarizes it. Measured at 0.00 bytes/op while the
  // return type was still a named tuple; this holds the line now that it is a `final class`, and would catch a
  // restructuring that lets the result escape. The `intarrays` twin is deliberately absent — its copy of
  // `meanAndVarianceTwoPass` allocates a lane-widening scratch buffer per call.
  test("D1: doublearrays.variance(mode)") {
    val arr = Array.tabulate(N)(i => (i % 100).toDouble)
    assertAllocFree("doublearrays.variance(mode)") {
      doubleSink = arr.variance(VarianceMode.Population)
    }
  }

  // A prefix sum carries a dependency, so it cannot be vectorised and there is no Vector API in the body at all.
  // That makes this the only @AllocFree kernel in the library whose zero is unconditional rather than contingent on
  // C2 applying intrinsics — worth having measured precisely because it is the control case for all the others.
  test("D1: doublearrays.cumsum!") {
    val arr = Array.fill(N)(1.0)
    assertAllocFree("doublearrays.cumsum!")(arr.`cumsum!`)
  }

  // ── Float ───────────────────────────────────────────────────────────────────

  test("D1: floatarrays.sumSIMD") {
    val arr = Array.tabulate(N)(_.toFloat)
    assertAllocFree("floatarrays.sumSIMD") { floatSink = arr.sumSIMD }
  }

  test("D1: floatarrays.productSIMD") {
    val arr = Array.fill(N)(1.0001f)
    assertAllocFree("floatarrays.productSIMD") { floatSink = arr.productSIMD }
  }

  test("D1: floatarrays.fma!") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFree("floatarrays.fma!")(arr.`fma!`(2.0f, 0.5f))
  }

  test("D1: floatarrays.+=(Float)") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFree("floatarrays.+=(Float)")(arr += 0.1f)
  }

  test("D1: floatarrays.-=(Float)") {
    val arr = Array.fill(N)(5.0f)
    assertAllocFree("floatarrays.-=(Float)")(arr -= 0.1f)
  }

  test("D1: floatarrays.-!") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFree("floatarrays.-!")(arr.`-!`)
  }

  test("D1: floatarrays.abs!") {
    val arr = Array.tabulate(N)(i => if i % 2 == 0 then i.toFloat else -i.toFloat)
    assertAllocFree("floatarrays.abs!")(arr.`abs!`)
  }

  test("D1: floatarrays.clamp!") {
    val arr = Array.tabulate(N)(i => (i % 10).toFloat)
    assertAllocFree("floatarrays.clamp!")(arr.`clamp!`(2.0f, 7.0f))
  }

  test("D1: floatarrays.+=(Array[Float])") {
    val arr = Array.fill(N)(1.0f)
    val arr2 = Array.fill(N)(0.0f) // adding zero keeps the values stable across the measurement windows
    assertAllocFree("floatarrays.+=(Array[Float])")(arr += arr2)
  }

  test("D1: floatarrays.*=(Array[Float])") {
    val arr = Array.fill(N)(2.0f)
    // See doublearrays.*= — a multiplier of 1.0 avoids driving the array to denormals and then to
    // zero partway through, which would make most of the measured workload operate on zeros.
    val arr2 = Array.fill(N)(1.0f)
    assertAllocFree("floatarrays.*=(Array[Float])")(arr *= arr2)
  }

  test("D1: floatarrays.*=(Float)") {
    val arr = Array.fill(N)(2.0f)
    assertAllocFree("floatarrays.*=(Float)")(arr *= 1.0f)
  }

  test("D1: floatarrays.cumsum!") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFree("floatarrays.cumsum!")(arr.`cumsum!`)
  }

  // ── Int ─────────────────────────────────────────────────────────────────────

  test("D1: intarrays.sumSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFree("intarrays.sumSIMD") { intSink = arr.sumSIMD }
  }

  // `dot` has carried @AllocFree since #107 without ever being measured, and it was not alloc-free: the body opened
  // with an `Array.ofDim[Int](vec.length)` that nothing read. Removing the dead allocation makes the annotation true;
  // this test is what stops it drifting back.
  test("D1: intarrays.dot") {
    val arr = Array.tabulate(N)(i => i % 100)
    val arr2 = Array.tabulate(N)(i => (i + 1) % 100)
    assertAllocFree("intarrays.dot") { intSink = arr.dot(arr2) }
  }

  test("D1: intarrays.minSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFree("intarrays.minSIMD") { intSink = arr.minSIMD }
  }

  test("D1: intarrays.maxSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFree("intarrays.maxSIMD") { intSink = arr.maxSIMD }
  }

  test("D1: intarrays.+=(Array[Int])") {
    val arr = Array.fill(N)(1)
    val arr2 = Array.fill(N)(0) // adding zero keeps the values from overflowing across the windows
    assertAllocFree("intarrays.+=(Array[Int])")(arr += arr2)
  }

  test("D1: intarrays.-=(Array[Int])") {
    val arr = Array.fill(N)(1)
    val arr2 = Array.fill(N)(0)
    assertAllocFree("intarrays.-=(Array[Int])")(arr -= arr2)
  }

  test("D1: intarrays.-=(Int)") {
    val arr = Array.fill(N)(1)
    assertAllocFree("intarrays.-=(Int)")(arr -= 0)
  }

end D1Suite
