package vecxt.jit

import munit.FunSuite
import vecxt.all.{*, given}

/** D1 EA-off cross-check — guards against undetected SIMD→software regression.
  *
  * Runs the same fourteen kernels as {@link D1Suite} with escape analysis disabled ({@code -XX:-DoEscapeAnalysis}).
  * This provides a check that is strictly stronger than D1Suite alone, for the following reason:
  *
  * '''Original design intent vs. reality:'''
  *
  * The original design assumed that EA was required for zero allocation in Vector API kernels (i.e. that EA scalarises
  * transient {@code Vector} objects), and that EA-off would therefore force those kernels to allocate ≥ 64 bytes/op. In
  * practice, HotSpot Vector API intrinsics lower {@code fromArray}/{@code add}/etc. to SIMD machine instructions at the
  * {@code VectorSupport} layer, bypassing heap allocation <em>entirely independently of EA</em>. A properly
  * intrinsified kernel allocates zero bytes regardless of whether EA is enabled or disabled, because the
  * {@code DoubleVector} objects never reach the heap in the first place.
  *
  * '''What this cross-check actually proves:'''
  *
  * Consider the two paths a Vector API kernel can follow:
  *   1. '''SIMD intrinsics path''' — C2 applies {@code VectorSupport} intrinsics; {@code Vector} objects are replaced
  *      by SIMD instructions; no heap allocation occurs regardless of EA.
  *   2. '''Software fallback path''' — intrinsics are not applied (wrong species, missing module, C2 profile-data loss,
  *      etc.); the Java {@code DoubleVector} methods run interpreted or via C1, creating real heap objects; EA normally
  *      scalarises these objects so D1Suite still passes; but with EA <em>disabled</em> the objects are heap-allocated
  *      and allocation is visible.
  *
  * D1Suite cannot distinguish paths (1) and (2) when EA is enabled, because EA masks the software fallback.
  * D1EAOffSuite asserts the same ≤ 8 bytes threshold under EA-off: if a kernel has regressed to the software fallback
  * path, D1EAOffSuite fails here while D1Suite would continue to pass.
  *
  * '''DCE guard:'''
  *
  * Pure reductions store their results into {@code @volatile} fields, preventing C2 from dead-code-eliminating the
  * computation (a volatile store is a visible side-effect that C2 cannot remove). This is the same technique used in
  * D1Suite and is independent of EA.
  *
  * Tests are skipped off-CI when {@code ThreadMXBean} allocation tracking is not available. On CI ({@code CI}
  * environment variable set) a missing bean is a hard failure.
  */
class D1EAOffSuite extends FunSuite:

  private val N = 1024
  private val Eps = 8L // bytes/op: same threshold as D1Suite
  private val Warmup = 20_000
  private val Reps = 100_000

  // Volatile sinks for pure-reduction results — prevents DCE independent of EA.
  @volatile private var doubleSink: Double = 0.0
  @volatile private var floatSink: Float = 0.0f
  @volatile private var intSink: Int = 0

  private inline def assertAllocFreeWithEAOff(label: String)(inline body: => Unit): Unit =
    val total = AllocMeter.measureAlloc(Warmup, Reps)(body)
    if total < 0L && sys.env.contains("CI") then
      fail(
        s"[D1-EAOff] $label: CI detected but ThreadMXBean allocation tracking is unavailable. " +
          "The pinned runner (Temurin 25) must support per-thread allocation tracking. " +
          "Check JVM configuration or the runner image."
      )
    end if
    assume(total >= 0L, s"[D1-EAOff] skip $label — ThreadMXBean allocation tracking not available on this JVM")
    val perOp = total.toDouble / Reps
    assert(
      perOp <= Eps,
      s"D1-EAOff github.com/Quafadas/vecxt/issues/105: $label allocated ${perOp.toLong} bytes/op " +
        s"with -XX:-DoEscapeAnalysis (total=$total over $Reps iterations). " +
        s"Expected ≤ $Eps bytes/op. A kernel allocating with EA disabled is on the software " +
        s"fallback path (SIMD intrinsics not applied). This would have been invisible in D1Suite " +
        s"because EA normally scalarises software-path Vector objects. " +
        s"Check that jdk.incubator.vector is on the module path, that species is from a " +
        s"static-final field (not a method parameter), and that C2 is applying VectorSupport " +
        s"intrinsics to this kernel."
    )
  end assertAllocFreeWithEAOff

  // ── Double ──────────────────────────────────────────────────────────────────

  test("D1-EAOff: doublearrays.sumSIMD") {
    val arr = Array.tabulate(N)(_.toDouble)
    assertAllocFreeWithEAOff("doublearrays.sumSIMD") { doubleSink = arr.sumSIMD }
  }

  test("D1-EAOff: doublearrays.productSIMD") {
    val arr = Array.fill(N)(1.0001)
    assertAllocFreeWithEAOff("doublearrays.productSIMD") { doubleSink = arr.productSIMD }
  }

  test("D1-EAOff: doublearrays.+=(Double)") {
    val arr = Array.fill(N)(1.0)
    assertAllocFreeWithEAOff("doublearrays.+=(Double)")(arr += 0.1)
  }

  test("D1-EAOff: doublearrays.-=(Double)") {
    val arr = Array.fill(N)(5.0)
    assertAllocFreeWithEAOff("doublearrays.-=(Double)")(arr -= 0.1)
  }

  test("D1-EAOff: doublearrays.*=(Array[Double])") {
    val arr = Array.fill(N)(2.0)
    val arr2 = Array.fill(N)(1.0)
    assertAllocFreeWithEAOff("doublearrays.*=(Array[Double])")(arr *= arr2)
  }

  test("D1-EAOff: doublearrays.fma!") {
    val arr = Array.fill(N)(1.0)
    assertAllocFreeWithEAOff("doublearrays.fma!")(arr.`fma!`(2.0, 0.5))
  }

  test("D1-EAOff: doublearrays.clamp!") {
    val arr = Array.tabulate(N)(i => (i % 10).toDouble)
    assertAllocFreeWithEAOff("doublearrays.clamp!")(arr.`clamp!`(2.0, 7.0))
  }

  test("D1-EAOff: doublearrays.fillLinspace") {
    val dest = new Array[Double](N)
    assertAllocFreeWithEAOff("doublearrays.fillLinspace")(fillLinspace(dest, 0.0, 1.0))
  }

  // ── Float ───────────────────────────────────────────────────────────────────

  test("D1-EAOff: floatarrays.sumSIMD") {
    val arr = Array.tabulate(N)(_.toFloat)
    assertAllocFreeWithEAOff("floatarrays.sumSIMD") { floatSink = arr.sumSIMD }
  }

  test("D1-EAOff: floatarrays.productSIMD") {
    val arr = Array.fill(N)(1.0001f)
    assertAllocFreeWithEAOff("floatarrays.productSIMD") { floatSink = arr.productSIMD }
  }

  test("D1-EAOff: floatarrays.fma!") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFreeWithEAOff("floatarrays.fma!")(arr.`fma!`(2.0f, 0.5f))
  }

  test("D1-EAOff: floatarrays.+=(Float)") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFreeWithEAOff("floatarrays.+=(Float)")(arr += 0.1f)
  }

  test("D1-EAOff: floatarrays.-=(Float)") {
    val arr = Array.fill(N)(5.0f)
    assertAllocFreeWithEAOff("floatarrays.-=(Float)")(arr -= 0.1f)
  }

  // ── Int ─────────────────────────────────────────────────────────────────────

  test("D1-EAOff: intarrays.sumSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFreeWithEAOff("intarrays.sumSIMD") { intSink = arr.sumSIMD }
  }

end D1EAOffSuite
