package vecxt_re

import AmountUnit.*
import Ccy.*

class CurrencyAmountSuite extends munit.FunSuite:

  test("construction scales by unit"):
    assertEquals(CurrencyAmount(2.5, One, USD).amount, 2.5)
    assertEquals(CurrencyAmount(2.5, K, USD).amount, 2_500.0)
    assertEquals(CurrencyAmount(2.5, Mn, USD).amount, 2_500_000.0)
    assertEquals(CurrencyAmount(2.5, Bn, USD).amount, 2_500_000_000.0)

  test("the same money in different units is equal"):
    assertEquals(CurrencyAmount(1, Mn, EUR), CurrencyAmount(1_000, K, EUR))
    assertEquals(CurrencyAmount(1, Bn, EUR), CurrencyAmount(1_000_000_000, One, EUR))
    assertEquals(CurrencyAmount(1, Mn, EUR).hashCode, CurrencyAmount(1_000, K, EUR).hashCode)

  test("the same number in different currencies is not equal"):
    assertNotEquals(CurrencyAmount(1, Mn, USD), CurrencyAmount(1, Mn, EUR))

  test("in converts to the requested unit"):
    val x = CurrencyAmount(2.5, Mn, USD)
    assertEquals(x.in(One), 2_500_000.0)
    assertEquals(x.in(K), 2_500.0)
    assertEquals(x.in(Mn), 2.5)
    assertEqualsDouble(x.in(Bn), 0.0025, 1e-15)

  test("addition and subtraction across units"):
    val x = CurrencyAmount(1, Mn, GBP)
    val y = CurrencyAmount(250, K, GBP)
    assertEquals(x + y, CurrencyAmount(1.25, Mn, GBP))
    assertEquals(x - y, CurrencyAmount(750, K, GBP))
    assertEquals(y - x, CurrencyAmount(-750, K, GBP))

  test("negation"):
    assertEquals(-CurrencyAmount(3, K, CHF), CurrencyAmount(-3, K, CHF))
    assertEquals(-(-CurrencyAmount(3, K, CHF)), CurrencyAmount(3, K, CHF))

  test("zero is the additive identity"):
    val x = CurrencyAmount(42, Mn, JPY)
    assertEquals(x + CurrencyAmount.zero(JPY), x)
    assertEquals(x - x, CurrencyAmount.zero(JPY))

  test("arithmetic between currencies fails"):
    val usd = CurrencyAmount(1, Mn, USD)
    val eur = CurrencyAmount(1, Mn, EUR)
    intercept[IllegalArgumentException](usd + eur)
    intercept[IllegalArgumentException](usd - eur)
    intercept[IllegalArgumentException](usd / eur)

  test("premium = notional * coupon, in any price unit"):
    val notional = CurrencyAmount(100, Mn, USD)
    val expected = CurrencyAmount(500, K, USD)
    assertEquals(notional * Rel(50, PriceUnit.Bps), expected)
    assertEquals(notional * Rel(0.5, PriceUnit.Pts), expected)
    assertEquals(notional * Rel(0.005, PriceUnit.One), expected)
    assertEquals(notional * Spread(50, PriceUnit.Bps), expected)

  test("scaling by a Double keeps the currency"):
    val x = CurrencyAmount(10, Mn, AUD) * 0.3
    assertEquals(x.ccy, AUD)
    assert(x.approxEq(CurrencyAmount(3, Mn, AUD)))

  test("dividing two amounts gives a Rel"):
    val loss = CurrencyAmount(60, Mn, USD)
    val premium = CurrencyAmount(80, Mn, USD)
    assertEquals(loss / premium, Rel(75, PriceUnit.Pts))
    assertEquals(premium / premium, Rel.one)

  test("dividing then multiplying round trips"):
    val a = CurrencyAmount(123.456, K, NZD)
    val b = CurrencyAmount(7.89, Mn, NZD)
    assert((b * (a / b)).approxEq(a))

  test("approxEq tolerates floating point noise"):
    val sum = CurrencyAmount(0.1, One, USD) + CurrencyAmount(0.2, One, USD)
    val exact = CurrencyAmount(0.3, One, USD)
    assertNotEquals(sum, exact)
    assert(sum.approxEq(exact))

  test("approxEq respects the tolerance"):
    val x = CurrencyAmount(1, Mn, USD)
    val y = CurrencyAmount(1_000_000.01, One, USD)
    assert(!x.approxEq(y))
    assert(x.approxEq(y, tol = 0.1))

  test("approxEq across currencies fails"):
    intercept[IllegalArgumentException](CurrencyAmount(1, Mn, USD).approxEq(CurrencyAmount(1, Mn, EUR)))

  test("ordering is by amount regardless of the unit used to construct"):
    val amounts = List(CurrencyAmount(2, Mn, CAD), CurrencyAmount(500, K, CAD), CurrencyAmount(0.001, Bn, CAD))
    assertEquals(
      amounts.sorted,
      List(CurrencyAmount(500, K, CAD), CurrencyAmount(1, Mn, CAD), CurrencyAmount(2, Mn, CAD))
    )
    assertEquals(amounts.max, CurrencyAmount(2, Mn, CAD))

  test("ordering across currencies fails"):
    intercept[IllegalArgumentException](List(CurrencyAmount(1, Mn, USD), CurrencyAmount(1, Mn, EUR)).sorted)

  test("very large amounts do not overflow"):
    val x = CurrencyAmount(1_000_000, Bn, USD)
    assertEquals((x + x).in(Bn), 2_000_000.0)

  test("toString shows the amount in major units with the currency"):
    assertEquals(CurrencyAmount(2.5, Mn, USD).toString, "2500000.00 USD")
    assertEquals(CurrencyAmount(-1.234, K, EUR).toString, "-1234.00 EUR")

end CurrencyAmountSuite
