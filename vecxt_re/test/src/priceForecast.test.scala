package vecxt_re

import java.time.LocalDate
import vecxt.all.+

class PositionCalculationsSuite extends munit.FunSuite:
  
  test("one year maturity"):
    val reportDate = LocalDate.of(2026, 1, 1)
    val forecastPrice = PositionCalculations.priceForecast1Year(102, PriceUnit.Pts, reportDate,reportDate.plusYears(1))

    assertEqualsDouble(forecastPrice, 100.0, 0.0000001)

  test("Maturity same date"):
    val reportDate = LocalDate.of(2026, 1, 1)
    val forecastPrice = PositionCalculations.priceForecast1Year(102, PriceUnit.Pts, reportDate,reportDate)

    assertEqualsDouble(forecastPrice, 100.0, 0.0000001)
    
  test("Early maturity"):
    val reportDate = LocalDate.of(2026, 1, 1)
    val forecastPrice = PositionCalculations.priceForecast1Year(102, PriceUnit.Pts, reportDate,reportDate.plusMonths(6))
    val forecastPrice2 = PositionCalculations.priceForecast1Year(102, PriceUnit.Pts, reportDate,reportDate.plusDays(1))

    assertEqualsDouble(forecastPrice, 100.0, 0.0000001)
    assertEqualsDouble(forecastPrice2, 100.0, 0.0000001)
  
  test("refuses already matured"): 
    val reportDate = LocalDate.of(2026, 1, 1)
    intercept[java.lang.AssertionError](PositionCalculations.priceForecast1Year(102, PriceUnit.Pts, reportDate,reportDate.plusYears(-1)))
      
  test("Interpolation to a point"):
    val reportDate = LocalDate.of(2026, 1, 1)
    val forecastPrice = PositionCalculations.priceForecast1Year(10200, PriceUnit.Bps, reportDate, reportDate.plusYears(2))    

    assertEqualsDouble(forecastPrice, 10100.0, 0.0000001)    

  test("pnlForecast"):

    val reportDate = LocalDate.of(2026, 1, 1)
    val losses = Array[Double](0.0, 0.1, 0.0, 0.2)
    val forecastPrice = PositionCalculations.pnlForecast1Year(
      102, PriceUnit.Pts, 
      reportDate, reportDate.plusYears(2), losses, 325.0, PriceUnit.Bps, 0, PriceUnit.Bps )

    val forecast = Array[Double](0.0225, -0.0775, 0.0225, -0.1775)
    assertVecEquals(forecastPrice, forecast )

    val forecastPriceWSpread = PositionCalculations.pnlForecast1Year(
      102, PriceUnit.Pts, 
      reportDate, reportDate.plusYears(2), losses, 325.0, PriceUnit.Bps, 100.0, PriceUnit.Bps )
    assertVecEquals(forecastPriceWSpread, forecast + 0.01 )
  
