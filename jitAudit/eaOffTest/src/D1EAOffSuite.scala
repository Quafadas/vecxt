package vecxt.jit

import munit.FunSuite
import vecxt.all.{*, given}

/** D1 EA-off cross-check — guards against undetected SIMD→software regression.
  *
  * Runs {@link D1Suite}'s kernels with escape analysis disabled ({@code -XX:-DoEscapeAnalysis}), plus a canary for the
  * flag itself. This is a check D1Suite cannot make, for the reason set out below.
  *
  * <p>Coverage is every {@code @AllocFree} kernel except {@code variance(mode)}, which appears here as the flag canary
  * instead — see its comment. The two suites cannot be factored into a shared collection: the assertion helpers have to
  * stay {@code inline} so each call site gets a monomorphic measurement loop, and driving the kernels from a
  * {@code List[() => Unit]} would reintroduce the megamorphic {@code Function0.apply()} dispatch that stops C2 inlining
  * through to the Vector API calls. So the two lists are kept in step by hand, and drifting apart is a real hazard:
  * this suite sat at fourteen kernels while D1Suite grew to twenty-nine.
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
  * <p>How much that is worth depends on how reliably EA rescues a software-path kernel, and the honest answer is "not
  * always". D6's canary — species from a method parameter — is a software-path kernel that allocates with EA
  * <em>enabled</em>, so D1Suite catches it unaided. The gap this suite closes is therefore narrower than "detects lost
  * intrinsification": it is the fallbacks whose objects happen not to escape, which EA removes and D1Suite then reads
  * as a pass. Worth having, and cheap, but not a substitute for confirming intrinsification directly.
  *
  * '''Why the flag canary is not optional:'''
  *
  * Every kernel assertion here reads "still zero with EA off", which is exactly what a run with EA still <em>on</em>
  * would produce. Without a test that fails when the flag is absent, this entire scope passes whether or not
  * {@code -XX:-DoEscapeAnalysis} reached the JVM — the same "silently became a no-op" hazard D6 exists to prevent for
  * D1Suite. The canary is the first test in the file.
  *
  * '''DCE guard:'''
  *
  * Pure reductions store their results into {@code @volatile} fields, preventing C2 from dead-code-eliminating the
  * computation (a volatile store is a visible side-effect that C2 cannot remove). This is the same technique used in
  * D1Suite and is independent of EA.
  *
  * The slope approach in {@link AllocMeter} (see its class-level Scaladoc) applies here too: a bounded burst from the
  * EA-off run lands in the first measurement window and cancels in the slope, so only steady-state allocation is
  * reported.
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

  /** The inverted assertion, used by exactly one test: the flag canary below.
    *
    * Every other assertion in this suite reads "still zero with EA off", which is indistinguishable from "the flag was
    * silently dropped and EA is on". This is the assertion that tells those two apart.
    */
  private inline def assertAllocatesWithEAOff(label: String)(inline body: => Unit): Unit =
    val total = AllocMeter.measureAlloc(Warmup, Reps)(body)
    if total < 0L && sys.env.contains("CI") then
      fail(s"[D1-EAOff] $label: CI detected but ThreadMXBean allocation tracking is unavailable.")
    end if
    assume(total >= 0L, s"[D1-EAOff] skip $label — ThreadMXBean allocation tracking not available on this JVM")
    val perOp = total.toDouble / Reps
    assert(
      perOp > Eps,
      s"D1-EAOff github.com/Quafadas/vecxt/issues/105: $label allocated ${perOp.toLong} bytes/op with " +
        s"-XX:-DoEscapeAnalysis, and was expected to allocate. This is the flag canary, not a kernel check: " +
        s"the allocation it looks for is one escape analysis would have removed, so measuring zero here means " +
        s"EA is still on and -XX:-DoEscapeAnalysis did not take effect. Every other assertion in this suite " +
        s"is then passing for the wrong reason. Check this scope's forkArgs before believing any of them."
    )
  end assertAllocatesWithEAOff

  // ── The flag canary ─────────────────────────────────────────────────────────

  /** Proves `-XX:-DoEscapeAnalysis` reached the JVM. Without this the whole scope is unfalsifiable — the absence of
    * such a check is the most likely reason the CI step for it sat commented out.
    *
    * `variance(mode)` is the one kernel in D1Suite whose zero comes from escape analysis rather than from
    * intrinsification. Its body reads one field out of the [[vecxt.MeanAndVariance]] that `meanAndVarianceTwoPass`
    * returns and discards the other, so the object is dead and EA removes it — D1Suite measures 0 bytes/op. That object
    * is an ordinary `final class`, not a `Vector`, so nothing intrinsifies it away. With EA off it must reach the heap.
    *
    * Which is also why it is absent from the kernel assertions below rather than merely inverted here: asserting
    * "≤ 8 bytes/op with EA off" for an EA-dependent kernel would be asserting the opposite of what the flag does.
    */
  test("D1-EAOff canary: doublearrays.variance(mode) must allocate with EA off") {
    val arr = Array.tabulate(N)(i => (i % 100).toDouble)
    assertAllocatesWithEAOff("doublearrays.variance(mode)") {
      doubleSink = arr.variance(VarianceMode.Population)
    }
  }

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

  // The two in-place unary kernels. NEG and ABS are intrinsified lanewise operations and the masked
  // tail's VectorMask is `_VectorFromBitsCoerced`, so nothing here depends on EA — which is exactly
  // what makes them worth asserting with EA off. The transcendentals (exp!, log!, …) are not
  // annotated @AllocFree and so are not measured in either suite.
  test("D1-EAOff: doublearrays.-!") {
    val arr = Array.fill(N)(1.0)
    assertAllocFreeWithEAOff("doublearrays.-!")(arr.`-!`)
  }

  test("D1-EAOff: doublearrays.abs!") {
    val arr = Array.tabulate(N)(i => if i % 2 == 0 then i.toDouble else -i.toDouble)
    assertAllocFreeWithEAOff("doublearrays.abs!")(arr.`abs!`)
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

  test("D1-EAOff: floatarrays.-!") {
    val arr = Array.fill(N)(1.0f)
    assertAllocFreeWithEAOff("floatarrays.-!")(arr.`-!`)
  }

  test("D1-EAOff: floatarrays.abs!") {
    val arr = Array.tabulate(N)(i => if i % 2 == 0 then i.toFloat else -i.toFloat)
    assertAllocFreeWithEAOff("floatarrays.abs!")(arr.`abs!`)
  }

  test("D1-EAOff: floatarrays.clamp!") {
    val arr = Array.tabulate(N)(i => (i % 10).toFloat)
    assertAllocFreeWithEAOff("floatarrays.clamp!")(arr.`clamp!`(2.0f, 7.0f))
  }

  test("D1-EAOff: floatarrays.+=(Array[Float])") {
    val arr = Array.fill(N)(1.0f)
    val arr2 = Array.fill(N)(0.0f)
    assertAllocFreeWithEAOff("floatarrays.+=(Array[Float])")(arr += arr2)
  }

  test("D1-EAOff: floatarrays.*=(Array[Float])") {
    val arr = Array.fill(N)(2.0f)
    val arr2 = Array.fill(N)(1.0f)
    assertAllocFreeWithEAOff("floatarrays.*=(Array[Float])")(arr *= arr2)
  }

  test("D1-EAOff: floatarrays.*=(Float)") {
    val arr = Array.fill(N)(2.0f)
    assertAllocFreeWithEAOff("floatarrays.*=(Float)")(arr *= 1.0f)
  }

  // ── Int ─────────────────────────────────────────────────────────────────────

  test("D1-EAOff: intarrays.sumSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFreeWithEAOff("intarrays.sumSIMD") { intSink = arr.sumSIMD }
  }

  test("D1-EAOff: intarrays.dot") {
    val arr = Array.tabulate(N)(i => i % 100)
    val arr2 = Array.tabulate(N)(i => (i + 1) % 100)
    assertAllocFreeWithEAOff("intarrays.dot") { intSink = arr.dot(arr2) }
  }

  test("D1-EAOff: intarrays.minSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFreeWithEAOff("intarrays.minSIMD") { intSink = arr.minSIMD }
  }

  test("D1-EAOff: intarrays.maxSIMD") {
    val arr = Array.tabulate(N)(i => i % 1000)
    assertAllocFreeWithEAOff("intarrays.maxSIMD") { intSink = arr.maxSIMD }
  }

  test("D1-EAOff: intarrays.+=(Array[Int])") {
    val arr = Array.fill(N)(1)
    val arr2 = Array.fill(N)(0)
    assertAllocFreeWithEAOff("intarrays.+=(Array[Int])")(arr += arr2)
  }

  test("D1-EAOff: intarrays.-=(Array[Int])") {
    val arr = Array.fill(N)(1)
    val arr2 = Array.fill(N)(0)
    assertAllocFreeWithEAOff("intarrays.-=(Array[Int])")(arr -= arr2)
  }

  test("D1-EAOff: intarrays.-=(Int)") {
    val arr = Array.fill(N)(1)
    assertAllocFreeWithEAOff("intarrays.-=(Int)")(arr -= 0)
  }

end D1EAOffSuite
