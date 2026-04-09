package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class BroadcastSuite extends FunSuite:

  // ── sameShape ─────────────────────────────────────────────────────────────

  test("sameShape - equal shapes"):
    assert(sameShape(Array(2, 3), Array(2, 3)))

  test("sameShape - different rank"):
    assert(!sameShape(Array(3), Array(2, 3)))

  test("sameShape - same rank different size"):
    assert(!sameShape(Array(2, 3), Array(2, 4)))

  test("sameShape - scalars / empty"):
    assert(sameShape(Array.empty[Int], Array.empty[Int]))

  // ── broadcastShape ────────────────────────────────────────────────────────

  test("broadcastShape - same shape"):
    val out = broadcastShape(Array(3, 4), Array(3, 4))
    assertEquals(out.toSeq, Seq(3, 4))

  test("broadcastShape - scalar broadcast"):
    val out = broadcastShape(Array(1), Array(5))
    assertEquals(out.toSeq, Seq(5))

  test("broadcastShape - lower rank expands on left"):
    val out = broadcastShape(Array(3), Array(2, 3))
    assertEquals(out.toSeq, Seq(2, 3))

  test("broadcastShape - dim-1 expands"):
    val out = broadcastShape(Array(1, 4), Array(3, 4))
    assertEquals(out.toSeq, Seq(3, 4))

  test("broadcastShape - both sides expand"):
    val out = broadcastShape(Array(3, 1), Array(1, 4))
    assertEquals(out.toSeq, Seq(3, 4))

  test("broadcastShape - incompatible shapes throw"):
    intercept[BroadcastException](broadcastShape(Array(2, 3), Array(2, 4)))

  // ── broadcastTo ───────────────────────────────────────────────────────────

  test("broadcastTo - same shape is no-op"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = a.broadcastTo(Array(3))
    assertEquals(b.toArray.toSeq, Seq(1.0, 2.0, 3.0))

  test("broadcastTo - 1D to 2D row broadcast"):
    // shape [3] -> [2,3]: column-major means each row is a column in memory
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = a.broadcastTo(Array(3, 3))
    // stride for the broadcast dim (axis 0) should be 0
    assertEquals(b.strides(0), 0)
    assertEquals(b.shape.toSeq, Seq(3, 3))

  test("broadcastTo - incompatible target throws"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[BroadcastException](a.broadcastTo(Array(4)))

  test("broadcastTo - more dims than source throws"):
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    // Source has more dims than target → BroadcastException
    intercept[BroadcastException](a.broadcastTo(Array(3)))

  // ── broadcastPair ─────────────────────────────────────────────────────────

  test("broadcastPair - equal shapes unchanged"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(4.0, 5.0, 6.0), Array(3))
    val (a2, b2) = broadcastPair(a, b)
    assertEquals(a2.shape.toSeq, Seq(3))
    assertEquals(b2.shape.toSeq, Seq(3))

  test("broadcastPair - incompatible shapes throw"):
    val a = NDArray(Array(1.0, 2.0), Array(2))
    val b = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[BroadcastException](broadcastPair(a, b))

end BroadcastSuite
