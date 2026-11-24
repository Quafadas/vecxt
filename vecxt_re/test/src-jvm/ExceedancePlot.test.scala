package vecxt_re

import munit.FunSuite

class ExceedancePlotSuite extends FunSuite:

  test("layerAttachments uses occurrence terms for OEP and aggregate terms for AEP") {
    val l = Layer(
      layerName = Some("L1"),
      occLimit = Some(10.0),
      occRetention = Some(5.0),
      aggLimit = Some(30.0),
      aggRetention = Some(2.0)
    )
    assertEquals(
      Plots.layerAttachments(l, ExceedanceType.OEP).toList,
      List((label = "L1 attach", loss = 5.0), (label = "L1 exhaust", loss = 15.0))
    )
    assertEquals(
      Plots.layerAttachments(l, ExceedanceType.AEP).toList,
      List((label = "L1 attach", loss = 2.0), (label = "L1 exhaust", loss = 32.0))
    )
  }

  test("unnamed layers are labelled in the plot's unit") {
    val l = Layer(limit = 100e6, ret = 50e6)
    assertEquals(
      Plots.layerAttachments(l, ExceedanceType.OEP, AmountUnit.Mn).map(_.label).toList,
      List("100 xs 50 attach", "100 xs 50 exhaust")
    )
    val agg = Layer(aggLimit = Some(1.5e6))
    assertEquals(
      Plots.layerAttachments(agg, ExceedanceType.AEP, AmountUnit.Mn).map(_.label).toList,
      List("1.5 xs 0 agg attach", "1.5 xs 0 agg exhaust")
    )
  }

  test("layerAttachments skips missing terms and reverse franchise exhaustion") {
    val aggOnly = Layer(aggLimit = Some(30.0), aggRetention = Some(2.0))
    assertEquals(Plots.layerAttachments(aggOnly, ExceedanceType.OEP), Seq.empty)
    val rf = Layer(occLimit = Some(10.0), occRetention = Some(5.0), occType = DeductibleType.ReverseFranchise)
    assertEquals(Plots.layerAttachments(rf, ExceedanceType.OEP).map(_.loss).toList, List(5.0))
    val noRet = Layer(occLimit = Some(10.0))
    assertEquals(Plots.layerAttachments(noRet, ExceedanceType.OEP).map(_.loss).toList, List(0.0, 10.0))
  }
end ExceedancePlotSuite
