package vecxt_re

import PriceUnit.*

class RelSuite extends munit.FunSuite:

  test("construction converts to canonical"):
    assertEqualsDouble(Rel(102, Pts).canonical, 1.02, 1e-12)
    assertEqualsDouble(Rel(50, Bps).canonical, 0.005, 1e-12)
    assertEqualsDouble(Rel(0.7, One).canonical, 0.7, 1e-12)

  test("in reads the value back out in any unit"):
    val p = Price(102, Pts)
    assertEqualsDouble(p.in(Pts), 102.0, 1e-9)
    assertEqualsDouble(p.in(Bps), 10200.0, 1e-9)
    assertEqualsDouble(p.in(One), 1.02, 1e-12)

  test("equality and ordering ignore the quoted unit"):
    assert(Price(100, Pts) == Price(1.0, One))
    assert(Spread(100, Bps) == Spread(1, Pts))
    assertEquals(List(Price(101, Pts), Price(9900, Bps), Price(1.0, One)).sorted.map(_.in(Pts)), List(99.0, 100.0, 101.0))
    assertEquals(List(Spread(50, Bps), Spread(1, Pts)).max, Spread(1, Pts))

  test("arithmetic mixes quoted units and preserves the left operand's type"):
    val p: Price = Price(98, Pts) + Spread(200, Bps)
    assertEquals(p, Price(1.0, One))
    val s: Spread = Spread(300, Bps) - Rel(1, Pts)
    assertEqualsDouble(s.in(Bps), 200.0, 1e-9)
    val n: Price = -Price(2, Pts)
    assertEqualsDouble(n.in(Pts), -2.0, 1e-12)

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
end RelSuite
