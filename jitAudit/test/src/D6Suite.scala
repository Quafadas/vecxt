package vecxt.jit

import munit.FunSuite
import jdk.incubator.vector.DoubleVector

/** D6 — Harness canary.
  *
  * A deliberately non-intrinsifiable kernel ({@link D6Canary#sumWithSpeciesParam}) must allocate significantly under
  * D1's measurement. This test asserts the canary allocates, which proves that {@link AllocMeter} is working: if
  * {@code getThreadAllocatedBytes} returned zero for everything, the canary would fail first.
  *
  * What D6 does and does not prove:
  *   - It proves {@link AllocMeter} is live and capable of detecting allocation. A silent zero from the bean would be
  *     caught here because the canary allocates unconditionally.
  *   - It does NOT prove that each individual D1 kernel body executed. The D1 kernels that optimise best (fully
  *     inlined, fully scalarized) have no observable side effect beyond their return value. A dead-code elimination of
  *     such a body would produce zero allocation for the right reason (nothing ran) rather than the correct reason
  *     (vectors were scalarized). D6 cannot distinguish these cases.
  *   - The protection against D1 vacuous passes is the combination of {@code @volatile} result sinks (preventing DCE)
  *     and the EA-off cross-check in {@code D1EAOffSuite} (asserting bodies execute under
  *     {@code -XX:-DoEscapeAnalysis}).
  *
  * The minimum threshold is 64 bytes/op: one {@code DoubleVector} object per operation. In practice the canary
  * allocates far more than this because each loop iteration creates at least two {@code DoubleVector} objects ({@code
  * fromArray} + {@code add} result), and a 1024-element array with 4-lane SIMD has 256 loop iterations. The threshold
  * is kept conservative to be robust across hardware (AVX2 vs AVX-512 changes lane count but not whether allocation
  * occurs).
  *
  * This is the dynamic counterpart of the C7 static check (§1 of the plan): C7 detects species-from-parameter in
  * bytecode; D6 confirms the runtime consequence is real allocation.
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

    if total < 0L && sys.env.contains("CI") then
      fail(
        "[D6] CI detected but ThreadMXBean allocation tracking is unavailable. " +
          "The pinned runner (Temurin 25) must support per-thread allocation tracking. " +
          "Check JVM configuration or the runner image."
      )
    end if
    assume(
      total >= 0L,
      "[D6] SKIP: ThreadMXBean allocation tracking not available — " +
        "canary liveness cannot be confirmed on this JVM"
    )
    val perOp = total.toDouble / Reps
    assert(
      perOp >= MinAllocPerOp,
      s"D6 github.com/Quafadas/vecxt/issues/105: canary allocated only ${perOp.toLong} bytes/op " +
        s"(total=$total over $Reps reps), expected ≥ $MinAllocPerOp bytes/op. " +
        s"This means either: (a) C2 constant-folded the species parameter (unexpected, " +
        s"revisit the canary design), or (b) the allocation measurement is broken. " +
        s"If (b), every D1 test may be passing for the wrong reason."
    )
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
