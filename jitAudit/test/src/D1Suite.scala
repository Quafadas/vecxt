package vecxt.jit

import munit.FunSuite
import vecxt.all.{*, given}

/** D1 — Allocation-free kernels.
  *
  * Per {@code @AllocFree} kernel: warm up 20,000 iterations, measure 100,000 iterations, and assert that bytes/op is
  * below a small epsilon. For a correctly JIT-compiled kernel, C2's escape analysis eliminates the transient
  * {@code Vector} objects and the allocation count is genuinely zero.
  *
  * Epsilon = 8 bytes/op rationale: the TLAB accounting counter is exact, but the surrounding measurement infrastructure
  * (two calls to {@code getThreadAllocatedBytes}, thread-local state in the bean) contributes a small constant. In
  * practice this is zero on Temurin 25, but the epsilon provides one Object-header's worth of headroom. A
  * {@code DoubleVector} object failing to scalarize would be ≥ 64 bytes — an order of magnitude above the threshold.
  *
  * Tests are skipped (not failed) when {@code ThreadMXBean} allocation tracking is not supported. The pinned CI runner
  * (Temurin 25) always supports it; a local non-HotSpot JVM might not.
  */
class D1Suite extends FunSuite:

  private val N = 1024
  private val Eps = 8L // bytes/op; see class doc
  private val Warmup = 20_000
  private val Reps = 100_000

  private def assertAllocFree(label: String)(body: => Unit): Unit =
    val total = AllocMeter.measureAlloc(Warmup, Reps)(body)
    if total < 0L then println(s"[D1] skip $label — ThreadMXBean allocation tracking not available on this JVM")
    else
      val perOp = total.toDouble / Reps
      assert(
        perOp <= Eps,
        s"D1 github.com/Quafadas/vecxt/issues/105: $label allocated ${perOp.toLong} bytes/op " +
          s"(total $total bytes over $Reps iterations). " +
          s"A DoubleVector failing to scalarize is ≥ 64 bytes/op. " +
          s"Check that jdk.incubator.vector is on the module path and that " +
          s"the species field is static final, not a method parameter."
      )
    end if
  end assertAllocFree

  // ── Double ──────────────────────────────────────────────────────────────────

  test("D1: doublearrays.sumSIMD") {
    val arr = Array.tabulate(N)(_.toDouble)
    assertAllocFree("doublearrays.sumSIMD") { arr.sumSIMD; () }
  }

  test("D1: doublearrays.productSIMD") {
    // Values close to 1.0 to avoid overflow/underflow across many calls.
    val arr = Array.fill(N)(1.0001)
    assertAllocFree("doublearrays.productSIMD") { arr.productSIMD; () }
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
    val arr2 = Array.fill(N)(0.5)
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

  // ── Float ───────────────────────────────────────────────────────────────────

  test("D1: floatarrays.sumSIMD") {
    val arr = Array.tabulate(N)(_.toFloat)
    assertAllocFree("floatarrays.sumSIMD") { arr.sumSIMD; () }
  }

  test("D1: floatarrays.productSIMD") {
    val arr = Array.fill(N)(1.0001f)
    assertAllocFree("floatarrays.productSIMD") { arr.productSIMD; () }
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

  // ── Int ─────────────────────────────────────────────────────────────────────

  test("D1: intarrays.sumSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFree("intarrays.sumSIMD") { arr.sumSIMD; () }
  }

end D1Suite
