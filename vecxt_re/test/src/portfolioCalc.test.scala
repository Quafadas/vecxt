package vecxt_re

import java.time.LocalDate

import vecxt.all.*

import PortfolioCalc.*

class PortfolioCalcSuite extends munit.FunSuite:

  private def amt(v: Double, ccy: Ccy): CurrencyAmount = CurrencyAmount(v, AmountUnit.One, ccy)

  private val tol = 1e-9

  test("empty portfolio gives empty array"):
    assertEquals(day0MarketValues(Nil, Ccy.CHF, Map.empty).length, 0)

  test("positions in portfolio currency need no FX rate"):
    val result = day0MarketValues(
      List(
        (price = Price(102, PriceUnit.Pts), notional = amt(1_000_000, Ccy.CHF), name = "A", id = "1", unused = "???")
      ),
      Ccy.CHF,
      Map.empty
    )
    assertEquals(result.length, 1)
    assertEqualsDouble(result(0), 1_020_000.0, tol)

  test("applies FX rate, notional and canonical price, preserving order"):
    val positions = List(
      (id = "1", name = "A", price = Price(102, PriceUnit.Pts), notional = amt(1_000_000, Ccy.USD)),
      (id = "2", name = "B", price = Price(9500, PriceUnit.Bps), notional = amt(2_000_000, Ccy.EUR)),
      (id = "3", name = "C", price = Price(1.0, PriceUnit.One), notional = amt(500_000, Ccy.CHF))
    )
    val result = day0MarketValues(positions, Ccy.CHF, Map(Ccy.USD -> 0.9, Ccy.EUR -> 0.95))
    assertEquals(result.length, 3)
    assertEqualsDouble(result(0), 0.9 * 1_000_000 * 1.02, tol)
    assertEqualsDouble(result(1), 0.95 * 2_000_000 * 0.95, tol)
    assertEqualsDouble(result(2), 500_000.0, tol)

  test("field order does not matter and extra fields are ignored"):
    val result = day0MarketValues(
      List((desk = "rates", notional = amt(100, Ccy.USD), id = "1", price = Price(100, PriceUnit.Pts), name = "A")),
      Ccy.CHF,
      Map(Ccy.USD -> 2.0)
    )
    assertEqualsDouble(result(0), 200.0, tol)

  test("unused FX rates are ignored"):
    val result = day0MarketValues(
      List((price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.USD), name = "A", id = "1")),
      Ccy.CHF,
      Map(Ccy.USD -> 2.0, Ccy.JPY -> 0.006, Ccy.GBP -> 1.1)
    )
    assertEqualsDouble(result(0), 200.0, tol)

  test("missing or mistyped fields do not compile"):
    assertNoDiff(
      compileErrors(
        """day0MarketValues(List((price = Price(100, PriceUnit.Pts), name = "A", id = "1")), Ccy.CHF, Map.empty)"""
      ).linesIterator.find(_.startsWith("error:")).getOrElse(""),
      "error: Missing field: notional"
    )
    assertNoDiff(
      compileErrors(
        """day0MarketValues(List((price = Price(100, PriceUnit.Pts), notional = 100.0, name = "A", id = "1")), Ccy.CHF, Map.empty)"""
      ).linesIterator.find(_.startsWith("error:")).getOrElse(""),
      "error: Field has the wrong type: notional"
    )

  test("missing FX rate throws with position id and name"):
    val ex = intercept[IllegalArgumentException](
      day0MarketValues(
        List((id = "P1", name = "Foo Bond", price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.USD))),
        Ccy.CHF,
        Map.empty
      )
    )
    val lines = ex.getMessage.split("\n").toList
    assertEquals(lines.length, 2)
    assert(lines.head.contains("1 position(s)"), lines.head)
    assertEquals(lines(1), "Position id=P1 name=Foo Bond: no FX rate from USD to CHF")

  test("all failures are reported in one exception, one per line, in input order"):
    val positions = List(
      (id = "P1", name = "Foo", price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.USD)),
      (id = "P2", name = "Ok", price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.EUR)),
      (id = "P3", name = "Bar", price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.JPY)),
      (id = "P4", name = "Home", price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.CHF)),
      (id = "P5", name = "Baz", price = Price(100, PriceUnit.Pts), notional = amt(100, Ccy.USD))
    )
    val ex = intercept[IllegalArgumentException](
      day0MarketValues(positions, Ccy.CHF, Map(Ccy.EUR -> 0.95))
    )
    val lines = ex.getMessage.split("\n").toList
    assert(lines.head.contains("3 position(s)"), lines.head)
    assertEquals(
      lines.tail,
      List(
        "Position id=P1 name=Foo: no FX rate from USD to CHF",
        "Position id=P3 name=Bar: no FX rate from JPY to CHF",
        "Position id=P5 name=Baz: no FX rate from USD to CHF"
      )
    )

  private val t0 = LocalDate.of(2026, 1, 1)

  private def pos(price: Price, rf: Double, spread: Double, maturity: LocalDate, notional: CurrencyAmount, id: String) =
    PositionT1(
      Map(
        "price" -> price,
        "riskFree" -> RiskFreeRate(rf, PriceUnit.Bps),
        "spread" -> Spread(spread, PriceUnit.Bps),
        "maturity" -> maturity,
        "notional" -> notional,
        "name" -> s"Bond $id",
        "id" -> id
      )
    )

  private val t1Holdings = IndexedSeq(
    // at par, long dated: no pull to par, carry 5%, 900k CHF
    pos(Price(100, PriceUnit.Pts), 300, 200, LocalDate.of(2036, 1, 1), amt(1_000_000, Ccy.USD), "A"),
    // matures within the year: pulled fully to par, no carry, already CHF
    pos(Price(98, PriceUnit.Pts), 0, 0, LocalDate.of(2026, 7, 1), amt(500_000, Ccy.CHF), "B")
  )

  // two scenarios (rows) x two positions (columns); losses are positive fractions of notional
  private val losses = Matrix.fromRows[Double](Array(0.0, 0.2), Array(0.1, 0.0))

  test("marketValueForecast1Year: notional in portfolio ccy times (price T1 + carry - loss), per scenario and position"):
    val mv = marketValueForecast1Year(t1Holdings, losses, t0, Map(Ccy.USD -> 0.9), Ccy.CHF)
    assertEquals((mv.rows, mv.cols), (2, 2))
    assertEqualsDouble(mv(0, 0), 900_000 * 1.05, 1e-6)
    assertEqualsDouble(mv(1, 0), 900_000 * (1.05 - 0.1), 1e-6)
    assertEqualsDouble(mv(0, 1), 500_000 * (1.0 - 0.2), 1e-6)
    assertEqualsDouble(mv(1, 1), 500_000 * 1.0, 1e-6)

  test("relativeReturn1Year: canonical return on day 0 market value, per scenario and position"):
    val r = relativeReturn1Year(t1Holdings, losses, t0)
    assertEquals((r.rows, r.cols), (2, 2))
    assertEqualsDouble(r(0, 0), 0.05, 1e-12)
    assertEqualsDouble(r(1, 0), -0.05, 1e-12)
    assertEqualsDouble(r(0, 1), 0.8 / 0.98 - 1.0, 1e-12)
    assertEqualsDouble(r(1, 1), 1.0 / 0.98 - 1.0, 1e-12)

  test("relativeReturn1Year agrees with marketValueForecast1Year / day 0 market value - 1"):
    val fx = Map(Ccy.USD -> 0.9)
    val mv1 = marketValueForecast1Year(t1Holdings, losses, t0, fx, Ccy.CHF)
    val mv0 = Array(900_000 * 1.0, 500_000 * 0.98)
    val r = relativeReturn1Year(t1Holdings, losses, t0)
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(r(i, j), mv1(i, j) / mv0(j) - 1.0, 1e-12)
    end for

  test("relativeReturn1Year rejects non-positive prices, reporting all of them"):
    val bad = IndexedSeq(
      pos(Price(0, PriceUnit.Pts), 0, 0, LocalDate.of(2030, 1, 1), amt(1, Ccy.CHF), "Z"),
      t1Holdings(0),
      pos(Price(-5, PriceUnit.Pts), 0, 0, LocalDate.of(2030, 1, 1), amt(1, Ccy.CHF), "N")
    )
    val ex = intercept[IllegalArgumentException](relativeReturn1Year(bad, Matrix[Double](Array.fill(3)(0.0), 1, 3), t0))
    val lines = ex.getMessage.split("\n").toList
    assert(lines.head.contains("2 position(s)"), lines.head)
    assertEquals(lines.tail.map(_.takeWhile(_ != ':')), List("Position id=Z name=Bond Z", "Position id=N name=Bond N"))

  test("relativeReturn1Year rejects a loss matrix whose width does not match the holdings"):
    intercept[IllegalArgumentException](relativeReturn1Year(t1Holdings, Matrix[Double](Array.fill(3)(0.0), 1, 3), t0))

end PortfolioCalcSuite
