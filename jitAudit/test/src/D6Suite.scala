package vecxt.jit

import munit.FunSuite
import jdk.incubator.vector.DoubleVector

/** D6 — Harness canary.
  *
  * A deliberately non-intrinsifiable kernel ({@link D6Canary#sumWithSpeciesParam}) must allocate significantly under
  * D1's measurement. This test asserts the canary allocates, which proves two things simultaneously:
  *
  *   1. The allocation measurement infrastructure ({@link AllocMeter}) is working.
  *   2. If any real {@code @AllocFree} kernel stopped vectorizing correctly (and its D1 test started passing for the
  *      wrong reason — because the harness is broken), this canary test would fail first, providing an early warning.
  *
  * The minimum threshold is 64 bytes/op: one {@code DoubleVector} object per operation. In practice the canary
  * allocates far more than this because each loop iteration creates at least two {@code DoubleVector} objects ({@code
  * fromArray} + {@code add} result), and a 1024-element array with 4-lane SIMD has 256 loop iterations. The threshold
  * is kept conservative to be robust across hardware (AVX2 vs AVX-512 changes lane count but not whether allocation
  * occurs).
  *
  * This is the dynamic counterpart of the C7 static check (§1 of the plan): C7 detects species-from-parameter in
  * bytecode; D6 confirms the runtime consequence is real allocation.
  *
  * Per the plan: "A check that has never been observed to fail is a check that might not work." D6 is the check that
  * ensures D1 is one of the checks that works.
  */
class D6Suite extends FunSuite:

  private val N = 1024
  private val Warmup = 20_000
  private val Reps = 100_000

  /** Minimum bytes/op the canary must allocate to confirm measurement is working. Conservative: one DoubleVector header
    * (64 bytes). The actual allocation is orders of magnitude higher, but we only need to show the measurement is not
    * dead.
    */
  private val MinAllocPerOp = 64.0

  test("D6: canary kernel allocates (harness is live)") {
    val arr = Array.tabulate(N)(_.toDouble)
    val species = DoubleVector.SPECIES_PREFERRED

    val total = AllocMeter.measureAlloc(Warmup, Reps) {
      D6Canary.sumWithSpeciesParam(arr, species)
    }

    if total < 0L then
      // ThreadMXBean not available: skip rather than fail, with an explicit message.
      // On Temurin 25 this should never happen; on some JVMs allocation tracking is disabled.
      println(
        "[D6] SKIP: ThreadMXBean allocation tracking not available — " +
          "canary liveness cannot be confirmed on this JVM"
      )
    else
      val perOp = total.toDouble / Reps
      assert(
        perOp >= MinAllocPerOp,
        s"D6 github.com/Quafadas/vecxt/issues/105: canary allocated only ${perOp.toLong} bytes/op " +
          s"(total=$total over $Reps reps), expected ≥ $MinAllocPerOp bytes/op. " +
          s"This means either: (a) C2 constant-folded the species parameter (unexpected, " +
          s"revisit the canary design), or (b) the allocation measurement is broken. " +
          s"If (b), every D1 test may be passing for the wrong reason."
      )
    end if
  }

  test("D6: canary produces correct sum (allocating does not mean wrong)") {
    // Allocation failure doesn't imply numerical incorrectness — the canary result must be
    // right even though it allocates. This test asserts it produces the expected sum.
    val arr = Array.tabulate(N)(_.toDouble)
    val species = DoubleVector.SPECIES_PREFERRED
    val result = D6Canary.sumWithSpeciesParam(arr, species)
    // Expected: 0 + 1 + 2 + ... + 1023 = 1023 * 1024 / 2 = 523776
    val expected = (N.toLong * (N - 1) / 2).toDouble
    assertEqualsDouble(result, expected, 1.0, s"canary sum: expected $expected got $result")
  }

end D6Suite
