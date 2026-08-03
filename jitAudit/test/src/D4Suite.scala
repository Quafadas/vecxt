package vecxt.jit

import munit.FunSuite
import vecxt.all.{*, given}

/** D4 — SIMD vs scalar differential correctness.
  *
  * For each kernel, the SIMD implementation is run on one copy of the inputs and a simple sequential scalar reference
  * runs on an identical copy. The results are then compared.
  *
  * Two classes of kernel behave differently:
  *
  * '''In-place mutations''' ({@code +=}, {@code *=}, {@code fma!}, {@code clamp!}): Each element is transformed
  * independently of the others, so the order in which the SIMD lanes apply the operation does not matter. IEEE 754
  * requires each double-precision operation to be correctly rounded, so the result must be bit-identical to a scalar
  * reference that does the same operation on each element in order.
  *
  * '''Reductions''' ({@code sumSIMD}, {@code productSIMD}): Floating-point addition and multiplication are not
  * associative. A lane-parallel partial-sum tree (as SIMD reduction performs) and a left-to-right sequential
  * accumulation produce results that differ by a rounding error bounded by:
  * {{{
  *   |simd - scalar| / |scalar| ≤ 4 * n * ε_machine
  * }}}
  * where {@code n} is the array length and {@code ε_machine = 2^-52 ≈ 2.22e-16} for double. The factor of 4 provides
  * headroom for both paths accumulating rounding independently.
  *
  * This tolerance is the documented contract. A test that asserts bit-exactness between a SIMD reduction and a
  * sequential one is a test that is either not actually vectorising or is using inputs where both paths happen to agree
  * (all-equal values, exactly representable values, etc.). If such a test passes today, it is worth investigating — see
  * D4 note in the plan (§2).
  *
  * Cross-platform note (goal Quafadas/vecxt#3): JVM (N-lane SIMD), Scala Native (LLVM width), and JS (scalar) produce
  * three different sums for non-trivial inputs. A cross-platform reduction contract needs an explicit decision — accept
  * the documented tolerance, or pin a fixed accumulation strategy. That decision is escalated, not made here; these
  * tests cover the JVM path only.
  */
class D4Suite extends FunSuite:

  private val N = 1024

  // Machine epsilon for double precision
  private val EpsMachine = 2.220446049250313e-16

  /** Relative tolerance for reductions: 4 * n * ε_m. Derived from the standard floating-point error bound for an
    * n-element sum; the factor of 4 accounts for both accumulation paths (SIMD and scalar) having independent rounding.
    */
  private def reductionTol(n: Int): Double = 4.0 * n * EpsMachine

  private def assertBitIdentical(label: String, a: Array[Double], b: Array[Double]): Unit =
    require(a.length == b.length, s"length mismatch: ${a.length} vs ${b.length}")
    var i = 0
    while i < a.length do
      val ai = a(i)
      val bi = b(i)
      assert(
        java.lang.Double.doubleToRawLongBits(ai) == java.lang.Double.doubleToRawLongBits(bi),
        s"D4 $label: element $i differs — SIMD=${ai} scalar=${bi}. " +
          s"In-place element-wise mutations must be bit-identical to sequential scalar."
      )
      i += 1
    end while
  end assertBitIdentical

  private def assertBitIdenticalF(label: String, a: Array[Float], b: Array[Float]): Unit =
    require(a.length == b.length, s"length mismatch: ${a.length} vs ${b.length}")
    var i = 0
    while i < a.length do
      val ai = a(i)
      val bi = b(i)
      assert(
        java.lang.Float.floatToRawIntBits(ai) == java.lang.Float.floatToRawIntBits(bi),
        s"D4 $label: element $i differs — SIMD=${ai} scalar=${bi}."
      )
      i += 1
    end while
  end assertBitIdenticalF

  private def assertRelTol(label: String, simd: Double, scalar: Double, tol: Double): Unit =
    val absScalar = Math.abs(scalar)
    val diff = Math.abs(simd - scalar)
    val rel = if absScalar == 0.0 then diff else diff / absScalar
    assert(
      rel <= tol,
      s"D4 $label: relative difference $rel exceeds documented tolerance $tol " +
        s"(simd=$simd scalar=$scalar). " +
        s"Tolerance is 4 * n * ε_machine where n=$N and ε_machine=$EpsMachine. " +
        s"See D4 in github.com/Quafadas/vecxt/issues/105 for the derivation."
    )
  end assertRelTol

  // ── Double in-place mutations (must be bit-identical) ───────────────────────

  test("D4: doublearrays.+=(Double) is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => i.toDouble * 0.1 + 1.0)
    val simd = base.clone()
    val ref = base.clone()

    simd += 3.14
    var i = 0;
    while i < N do
      ref(i) += 3.14; i += 1
    end while

    assertBitIdentical("doublearrays.+=(Double)", simd, ref)
  }

  test("D4: doublearrays.-=(Double) is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => i.toDouble * 0.1 + 10.0)
    val simd = base.clone()
    val ref = base.clone()

    simd -= 1.5
    var i = 0;
    while i < N do
      ref(i) -= 1.5; i += 1
    end while

    assertBitIdentical("doublearrays.-=(Double)", simd, ref)
  }

  test("D4: doublearrays.*=(Array[Double]) is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => i.toDouble * 0.1 + 1.0)
    val mult = Array.tabulate(N)(i => (i % 7 + 1).toDouble * 0.3)
    val simd = base.clone()
    val ref = base.clone()

    simd *= mult
    var i = 0;
    while i < N do
      ref(i) *= mult(i); i += 1
    end while

    assertBitIdentical("doublearrays.*=(Array[Double])", simd, ref)
  }

  test("D4: doublearrays.fma! is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => i.toDouble * 0.01 + 0.5)
    val simd = base.clone()
    val ref = base.clone()
    val m = 2.5
    val a = 1.3

    simd.`fma!`(m, a)
    var i = 0;
    while i < N do
      ref(i) = Math.fma(ref(i), m, a); i += 1
    end while

    assertBitIdentical("doublearrays.fma!", simd, ref)
  }

  test("D4: doublearrays.clamp! is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => (i % 20).toDouble - 5.0)
    val simd = base.clone()
    val ref = base.clone()

    simd.`clamp!`(-2.0, 8.0)
    var i = 0
    while i < N do
      ref(i) = if ref(i) > 8.0 then 8.0 else if ref(i) < -2.0 then -2.0 else ref(i)
      i += 1
    end while

    assertBitIdentical("doublearrays.clamp!", simd, ref)
  }

  // ── Double reductions (tolerance documented above) ───────────────────────────

  test("D4: doublearrays.sumSIMD agrees with scalar within tolerance") {
    // Mixed values to exercise both positive and negative rounding, and to avoid the
    // degenerate case where all values are equal (both paths always agree in that case).
    val arr = Array.tabulate(N)(i => Math.sin(i.toDouble))
    val simd = arr.sumSIMD

    var scalar = 0.0
    var i = 0;
    while i < N do
      scalar += arr(i); i += 1
    end while

    assertRelTol("doublearrays.sumSIMD", simd, scalar, reductionTol(N))
  }

  test("D4: doublearrays.productSIMD agrees with scalar within tolerance") {
    // Values very close to 1.0 to avoid overflow/underflow; the product stays near 1.0.
    val arr = Array.tabulate(N)(i => 1.0 + (i % 5) * 1e-6)
    val simd = arr.productSIMD

    var scalar = 1.0
    var i = 0;
    while i < N do
      scalar *= arr(i); i += 1
    end while

    assertRelTol("doublearrays.productSIMD", simd, scalar, reductionTol(N))
  }

  // ── Float in-place mutations (must be bit-identical) ────────────────────────

  test("D4: floatarrays.fma! is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => i.toFloat * 0.01f + 0.5f)
    val simd = base.clone()
    val ref = base.clone()
    val m = 2.5f
    val a = 1.3f

    simd.`fma!`(m, a)
    var i = 0
    while i < N do
      // The SIMD path uses a fused multiply-add (single rounding). The library's scalar tail
      // was fixed to use Math.fma for consistency, so both paths must agree bit-for-bit.
      ref(i) = Math.fma(ref(i), m, a)
      i += 1
    end while

    assertBitIdenticalF("floatarrays.fma!", simd, ref)
  }

  test("D4: floatarrays.+=(Float) is bit-identical to scalar") {
    val base = Array.tabulate(N)(i => i.toFloat * 0.1f)
    val simd = base.clone()
    val ref = base.clone()

    simd += 3.14f
    var i = 0;
    while i < N do
      ref(i) += 3.14f; i += 1
    end while

    assertBitIdenticalF("floatarrays.+=(Float)", simd, ref)
  }

  // ── NaN / ±0.0 / ±Infinity edge-case coverage ────────────────────────────────
  // Per §2 D4 note: where an erased generic path reaches java.lang.Double.equals rather than
  // dcmpl, NaN and -0.0 semantics differ. These tests cover the comparison-bearing ops.

  test("D4: doublearrays.sumSIMD handles NaN input") {
    val arr = Array(1.0, Double.NaN, 3.0, 4.0)
    // The sum of any sequence containing NaN is NaN — both SIMD and scalar must agree.
    val result = arr.sumSIMD
    assert(result.isNaN, s"sumSIMD over NaN-containing array should return NaN, got $result")
  }

  test("D4: doublearrays.clamp! handles ±Infinity") {
    val arr = Array(Double.NegativeInfinity, -1.0, 0.0, 1.0, Double.PositiveInfinity)
    arr.`clamp!`(-2.0, 2.0)
    // Infinities are outside the clamp range; they should be replaced by the bounds.
    assertEqualsDouble(arr(0), -2.0, 0.0)
    assertEqualsDouble(arr(4), 2.0, 0.0)
  }

  test("D4: doublearrays.+=(Double) preserves -0.0") {
    // -0.0 + 0.0 = +0.0 in IEEE 754 (the sign of zero is lost).
    val arr = Array(-0.0)
    arr += 0.0
    // Both SIMD and scalar follow IEEE 754, so the result is +0.0.
    assert(
      !java.lang.Double.doubleToRawLongBits(arr(0)).equals(java.lang.Double.doubleToRawLongBits(-0.0)),
      s"Expected +0.0 after (-0.0 + 0.0) but got ${arr(0)} (bits=${java.lang.Double.doubleToRawLongBits(arr(0))})"
    )
  }

end D4Suite
