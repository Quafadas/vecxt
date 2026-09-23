package vecxt_re

class PriceUnitSuite extends munit.FunSuite:

  test("conversions"):
    assertEqualsDouble(PriceUnit.Bps.convert(10000, PriceUnit.One), 1.0, 0.00000001)
    assertEqualsDouble(PriceUnit.Pts.convert(100, PriceUnit.One), 1.0, 0.00000001)
    assertEqualsDouble(PriceUnit.One.convert(1, PriceUnit.Bps), 1e4, 0.00000001)
end PriceUnitSuite
