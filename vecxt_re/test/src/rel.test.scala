package vecxt_re

import PriceUnit.*

class RelSuite extends munit.FunSuite:

  test("construction converts to canonical"):
    assertEqualsDouble(Rel(102, Pts).canonical, 1.02, 1e-12)
    assertEqualsDouble(Rel(50, Bps).canonical, 0.005, 1e-12)
    assertEqualsDouble(Price(0.7, One).canonical, 0.7, 1e-12)

  test("in reads the value back out in any unit"):
    val p = Price(102, Pts)
    assertEqualsDouble(p.in(Pts), 102.0, 1e-9)
    assertEqualsDouble(p.in(Bps), 10200.0, 1e-9)
    assertEqualsDouble(p.in(One), 1.02, 1e-12)
    assertEqualsDouble(Spread(0.005, One).in(Bps), 50.0, 1e-9)

  test("equality and ordering ignore the quoted unit"):
    assert(Price(100, Pts) == Price(1.0, One))
    assert(Spread(100, Bps) == Spread(1, Pts))
    assertEquals(List(Price(101, Pts), Price(9900, Bps), Price.par).sorted.map(_.in(Pts)), List(99.0, 100.0, 101.0))
    assertEquals(List(Spread(50, Bps), Spread(1, Pts)).max, Spread(1, Pts))

  test("Price - Price is a Rel"):
    val r: Rel = Price(102, Pts) - Price(98, Pts)
    assertEqualsDouble(r.in(Pts), 4.0, 1e-9)

  test("Price +/- Rel is a Price"):
    val up: Price = Price(98, Pts) + Spread(200, Bps)
    assertEquals(up, Price.par)
    val down: Price = Price(98, Pts) - Rel(3, Pts)
    assertEqualsDouble(down.in(Pts), 95.0, 1e-9)

  test("Price / Price is a ratio"):
    assertEqualsDouble(Price(102, Pts) / Price(100, Pts), 1.02, 1e-12)

  test("same-type arithmetic keeps the type, mixed widens to Rel"):
    val s: Spread = Spread(300, Bps) - Spread(100, Bps)
    assertEqualsDouble(s.in(Bps), 200.0, 1e-9)
    val y: Rel = RiskFreeRate(325, Bps) + Spread(100, Bps)
    assertEqualsDouble(y.in(Bps), 425.0, 1e-9)
    val n: Spread = -Spread(2, Pts)
    assertEqualsDouble(n.in(Pts), -2.0, 1e-12)

  test("price levels cannot be combined like returns"):
    // controls: the snippets compile in this scope when the types line up
    assertEquals(compileErrors("Price(1, One) - Price(1, One)"), "")
    assertEquals(compileErrors("val s: Spread = Spread(1, Bps) + Spread(1, Bps)"), "")
    assert(compileErrors("Price(1, One) + Price(1, One)").nonEmpty)
    assert(compileErrors("val r: Rel = Price(1, One)").nonEmpty)
    assert(compileErrors("-Price(1, One)").nonEmpty)
    assert(compileErrors("Price(1, One) * 2.0").nonEmpty)
    assert(compileErrors("Rel.one + Price(1, One)").nonEmpty)

  test("mixed rates do not keep the left operand's type"):
    assert(compileErrors("val s: Spread = Spread(1, Bps) + RiskFreeRate(1, Bps)").nonEmpty)
    assert(compileErrors("val s: Spread = Spread(1, Bps) - Rel(1, Bps)").nonEmpty)

  test("scaling by a Double from either side preserves type"):
    val a: Spread = Spread(100, Bps) * 3
    val b: Spread = 3.0 * Spread(100, Bps)
    assertEqualsDouble(a.in(Bps), 300.0, 1e-9)
    assertEquals(a, b)

  test("Rel / Rel is a unit-free ratio"):
    assertEqualsDouble(Rel(500, Bps) / Rel(2.5, Pts), 2.0, 1e-12)
    assertEqualsDouble(Rel(0.05, One) / Rel(500, Bps), 1.0, 1e-12)

  test("Rel / Double scales and preserves type"):
    val s: Spread = Spread(500, Bps) / 4
    assertEqualsDouble(s.in(Bps), 125.0, 1e-9)

  test("approxEq respects the tolerance"):
    assert((Rel(0.1, One) + Rel(0.2, One)).approxEq(Rel(0.3, One)))
    assert(!Rel(1, Bps).approxEq(Rel(2, Bps)))
    assert(Rel(1, Bps).approxEq(Rel(2, Bps), tol = 1e-4))
  test("typed arrays are double[] at runtime and canonical does not copy"):
    val ps = Array(Price(102, Pts), Price(98, Pts))
    val ss = Array(Spread(100, Bps))
    assertEquals[Any, Any](ps.getClass, classOf[Array[Double]])
    assertEquals[Any, Any](ss.getClass, classOf[Array[Double]])
    assert(ps.canonical eq ps.asInstanceOf[AnyRef])
    assert(ss.canonical eq ss.asInstanceOf[AnyRef])

  test("array Price - Price is Array[Rel], Price +/- Rel is Array[Price]"):
    val p0 = Array(Price(98, Pts), Price(102, Pts))
    val p1 = Array(Price(100, Pts), Price(100, Pts))
    val change: Array[Rel] = p1 - p0
    assertVecEquals(change.canonical, Array(0.02, -0.02))
    val carried: Array[Price] =
      p0 + Array(Spread(200, Bps), Spread(100, Bps)) + Array(RiskFreeRate(1, Pts), Rel(0, One))
    assertVecEquals(carried.canonical, Array(1.01, 1.03))
    val back: Array[Price] = carried - Array(Rel(3, Pts), Rel(5, Pts))
    assertVecEquals(back.canonical, Array(0.98, 0.98))
    assertVecEquals(p1 / p0, Array(1.0 / 0.98, 1.0 / 1.02))

  test("array Rel arithmetic follows the scalar type rules"):
    val s: Array[Spread] = Array(Spread(100, Bps)) + Array(Spread(50, Bps))
    assertVecEquals(s.canonical, Array(0.015))
    val scaled: Array[Spread] = s * 2.0
    assertVecEquals(scaled.canonical, Array(0.03))
    val mixed: Array[Rel] = Array(RiskFreeRate(300, Bps)) - Array(Spread(100, Bps))
    assertVecEquals(mixed.canonical, Array(0.02))
    assertEquals(compileErrors("Array(Price.par) + Array(Price.par)").nonEmpty, true)
    assertEquals(
      compileErrors("val s: Array[Spread] = Array(Spread(1, Bps)) + Array(RiskFreeRate(1, Bps))").nonEmpty,
      true
    )
    assertEquals(compileErrors("val s: Array[Spread] = Array(Spread(1, Bps)) + Array(Spread(1, Bps))"), "")
end RelSuite
