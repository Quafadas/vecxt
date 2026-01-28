package vecxt_re

class ScenarioRISuite extends munit.FunSuite:

  test("firstLimit prefers occLimit when both present") {
    val layer = Layer(
      occLimit = Some(10.0),
      occRetention = Some(1.0),
      aggLimit = Some(20.0)
    )
    assertEqualsDouble(layer.firstLimit, 10.0, 0.0)
  }

  test("firstLimit is occLimit when only occLimit is present") {
    val layer = Layer(
      occLimit = Some(15.0),
      occRetention = Some(2.0),
      aggLimit = None
    )
    assertEqualsDouble(layer.firstLimit, 15.0, 0.0)
  }

  test("firstLimit falls back to aggLimit when occLimit is absent") {
    val layer = Layer(
      occLimit = None,
      occRetention = None,
      aggLimit = Some(25.0)
    )
    assertEqualsDouble(layer.firstLimit, 25.0, 0.0)
  }

  test("firstLimit is PositiveInfinity when no limits are present") {
    val layer = Layer(
      occLimit = None,
      occRetention = None,
      aggLimit = None
    )
    assertEqualsDouble(layer.firstLimit, Double.PositiveInfinity, 0.0)
  }

  test("Layer default construction") {
    val layer = Layer()

    // Check defaults
    assertEquals(layer.layerName, None)
    assertEquals(layer.aggLimit, None)
    assertEquals(layer.aggRetention, None)
    assertEquals(layer.aggType, DeductibleType.Retention)
    assertEquals(layer.occLimit, None)
    assertEquals(layer.occRetention, None)
    assertEquals(layer.occType, DeductibleType.Retention)
    assertEquals(layer.share, 1.0)
    assertEquals(layer.basePremiumAmount, None)
    assertEquals(layer.reinstatement, None)
  }

  test("Layer with specific values") {
    val layerId = scala.util.Random.nextLong()
    val layer = Layer(
      layerId = layerId,
      layerName = Some("Test Layer"),
      aggLimit = Some(1000000.0),
      aggRetention = Some(100000.0),
      occLimit = Some(500000.0),
      occRetention = Some(50000.0),
      share = 0.75,
      basePremiumAmount = Some(10000.0),
      basePremiumUnit = Some(1.0),
      reinstatement = Some(Array(0.5, 0.75, 1.0))
    )

    assertEquals(layer.layerId, layerId)
    assertEquals(layer.layerName, Some("Test Layer"))
    assertEquals(layer.aggLimit, Some(1000000.0))
    assertEquals(layer.aggRetention, Some(100000.0))
    assertEquals(layer.occLimit, Some(500000.0))
    assertEquals(layer.occRetention, Some(50000.0))
    assertEquals(layer.share, 0.75)
    assertEquals(layer.basePremiumAmount, Some(10000.0))
    assert(layer.reinstatement.isDefined)
    layer.reinstatement.map(
      assertVecEquals(_, Array(0.5, 0.75, 1.0))
    )
  }

  test("Layer string conversions") {
    val layer = Layer(
      aggLimit = Some(1000000.1),
      aggRetention = Some(100000.1),
      occLimit = Some(500000.1),
      occRetention = Some(50000.1),
      basePremiumAmount = Some(10000.1),
      basePremiumUnit = Some(1.1),
      brokerageAmount = Some(500.1),
      brokerageUnit = Some(0.05)
    )

    assertEquals(layer.aggLimitString, Some("1000000.1"))
    assertEquals(layer.aggDeductibleString, Some("100000.1"))
    assertEquals(layer.occLimitString, Some("500000.1"))
    assertEquals(layer.occDeductibleString, Some("50000.1"))
    assertEquals(layer.premimuAmountString, Some("10000.1"))
    assertEquals(layer.premiumUnitString, Some("1.1"))
    assertEquals(layer.brokerageAmountString, Some("500.1"))
    assertEquals(layer.brokerageUnitString, Some("0.05"))
  }

  test("Layer string conversions with None values") {
    val layer = Layer()

    assertEquals(layer.aggLimitString, None)
    assertEquals(layer.aggDeductibleString, None)
    assertEquals(layer.occLimitString, None)
    assertEquals(layer.occDeductibleString, None)
    assertEquals(layer.premimuAmountString, None)
    assertEquals(layer.premiumUnitString, None)
    assertEquals(layer.brokerageAmountString, None)
    assertEquals(layer.brokerageUnitString, None)
  }

  test("applyScale with basic scaling") {
    val originalLayer = Layer(
      aggLimit = Some(1000000.0),
      aggRetention = Some(100000.0),
      occLimit = Some(500000.0),
      occRetention = Some(50000.0),
      basePremiumUnit = Some(1.0),
      commissionUnit = Some(0.1),
      brokerageUnit = Some(0.05),
      taxUnit = Some(0.02),
      feeUnit = Some(1000.0)
    )

    val scaledLayer = originalLayer.applyScale(2.0)

    // Check that monetary limits are scaled
    assertEquals(scaledLayer.aggLimit, Some(2000000.0))
    assertEquals(scaledLayer.aggRetention, Some(200000.0))
    assertEquals(scaledLayer.occLimit, Some(1000000.0))
    assertEquals(scaledLayer.occRetention, Some(100000.0))

    // Check that units are scaled
    assertEquals(scaledLayer.basePremiumUnit, Some(2.0))
    assertEquals(scaledLayer.commissionUnit, Some(0.2))
    assertEquals(scaledLayer.brokerageUnit, Some(0.1))
    assertEquals(scaledLayer.taxUnit, Some(0.04))
    assertEquals(scaledLayer.feeUnit, Some(2000.0))

    // Check that amounts are preserved
    assertEquals(scaledLayer.basePremiumAmount, originalLayer.basePremiumAmount)
    assertEquals(scaledLayer.commissionAmount, originalLayer.commissionAmount)

    // Check that other fields are preserved
    assertEquals(scaledLayer.layerId, originalLayer.layerId)
    assertEquals(scaledLayer.share, originalLayer.share)
    assertEquals(scaledLayer.aggType, originalLayer.aggType)
    assertEquals(scaledLayer.occType, originalLayer.occType)
  }

  test("applyScale with zero scale") {
    val originalLayer = Layer(
      aggLimit = Some(1000000.0),
      occLimit = Some(500000.0),
      basePremiumUnit = Some(1.0)
    )

    val scaledLayer = originalLayer.applyScale(0.0)

    assertEquals(scaledLayer.aggLimit, Some(0.0))
    assertEquals(scaledLayer.occLimit, Some(0.0))
    assertEquals(scaledLayer.basePremiumUnit, Some(0.0))
  }

  test("applyScale with negative scale") {
    val originalLayer = Layer(
      aggLimit = Some(1000000.0),
      basePremiumUnit = Some(1.0)
    )

    val scaledLayer = originalLayer.applyScale(-1.0)

    assertEquals(scaledLayer.aggLimit, Some(-1000000.0))
    assertEquals(scaledLayer.basePremiumUnit, Some(-1.0))
  }

  test("applyScale with None values") {
    val originalLayer = Layer()
    val scaledLayer = originalLayer.applyScale(2.0)

    assertEquals(scaledLayer.aggLimit, None)
    assertEquals(scaledLayer.aggRetention, None)
    assertEquals(scaledLayer.occLimit, None)
    assertEquals(scaledLayer.occRetention, None)
    assertEquals(scaledLayer.basePremiumUnit, None)
  }

  test("applyScale description updates") {
    val originalLayer = Layer(
      basePremiumUnit = Some(100.1),
      basePremiumDescription = Some("Base premium"),
      commissionUnit = Some(0.1),
      commissionDescription = Some("Commission"),
      brokerageUnit = Some(0.05),
      brokerageDescription = Some("Brokerage")
    )

    val scaledLayer = originalLayer.applyScale(2.1)

    assertEquals(scaledLayer.basePremiumDescription, Some("Base premium at Some(100.1) * 2.1"))
    assertEquals(scaledLayer.commissionDescription, Some("Commission at Some(0.1) * 2.1"))
    assertEquals(scaledLayer.brokerageDescription, Some("Brokerage at Some(0.05) * 2.1"))
  }

  test("cap with retention deductible returns occLimit") {
    val layer = Layer(
      occLimit = Some(500000.0),
      occRetention = Some(50000.0),
      occType = DeductibleType.Retention
    )

    assertEquals(layer.cap, 550000.0)
  }

  test("cap with franchise adds retention") {
    val layer = Layer(
      occLimit = Some(500000.0),
      occRetention = Some(50000.0),
      occType = DeductibleType.Franchise
    )

    assertEquals(layer.cap, 500000.0)
  }

  test("cap with franchise and missing retention defaults to limit") {
    val layer = Layer(
      occLimit = Some(500000.0),
      occType = DeductibleType.Franchise
    )

    assertEquals(layer.cap, 500000.0)
  }

  test("cap with reverse franchise returns NaN") {
    val layer = Layer(
      occLimit = Some(500000.0),
      occRetention = Some(50000.0),
      occType = DeductibleType.ReverseFranchise
    )

    assert(java.lang.Double.isNaN(layer.cap))
  }

  test("cap without occLimit returns positive infinity") {
    val layer = Layer()

    assertEquals(layer.cap, Double.PositiveInfinity)
  }

  test("occLayer sublayer creation") {
    val layer = Layer(
      occLimit = Some(500000.0),
      occRetention = Some(50000.0),
      occType = DeductibleType.Retention
    )

    val occLayer = layer.occLayer

    assertEquals(occLayer.limit, Some(500000.0))
    assertEquals(occLayer.retention, Some(50000.0))
    assertEquals(occLayer.layerType, LossCalc.Occ)
    assertEquals(occLayer.aggOrOcc, DeductibleType.Retention)
  }

  test("aggLayer sublayer creation") {
    val layer = Layer(
      aggLimit = Some(1000000.0),
      aggRetention = Some(100000.0),
      aggType = DeductibleType.Retention
    )

    val aggLayer = layer.aggLayer

    assertEquals(aggLayer.limit, Some(1000000.0))
    assertEquals(aggLayer.retention, Some(100000.0))
    assertEquals(aggLayer.layerType, LossCalc.Agg)
    assertEquals(aggLayer.aggOrOcc, DeductibleType.Retention)
  }

  test("sublayers with None values") {
    val layer = Layer()

    val occLayer = layer.occLayer
    assertEquals(occLayer.limit, None)
    assertEquals(occLayer.retention, None)
    assertEquals(occLayer.layerType, LossCalc.Occ)

    val aggLayer = layer.aggLayer
    assertEquals(aggLayer.limit, None)
    assertEquals(aggLayer.retention, None)
    assertEquals(aggLayer.layerType, LossCalc.Agg)
  }

  test("Layer immutability after scaling") {
    val originalLayer = Layer(
      aggLimit = Some(1000000.0),
      basePremiumUnit = Some(1.0)
    )

    val scaledLayer = originalLayer.applyScale(2.0)

    // Original should be unchanged
    assertEquals(originalLayer.aggLimit, Some(1000000.0))
    assertEquals(originalLayer.basePremiumUnit, Some(1.0))

    // Scaled should be different
    assertEquals(scaledLayer.aggLimit, Some(2000000.0))
    assertEquals(scaledLayer.basePremiumUnit, Some(2.0))

    // Should be different instances
    assert(originalLayer != scaledLayer)
  }

  test("Sublayer case class") {
    val sublayer = Sublayer(
      limit = Some(1000000.0),
      retention = Some(100000.0),
      layerType = LossCalc.Agg,
      aggOrOcc = DeductibleType.Retention
    )

    assertEquals(sublayer.limit, Some(1000000.0))
    assertEquals(sublayer.retention, Some(100000.0))
    assertEquals(sublayer.layerType, LossCalc.Agg)
    assertEquals(sublayer.aggOrOcc, DeductibleType.Retention)
  }

end ScenarioRISuite
