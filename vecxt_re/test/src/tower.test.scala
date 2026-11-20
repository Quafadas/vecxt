package vecxt_re

import vecxt_re.*
import vecxt.all.*
import SplitLosses.*

class TowerSuite extends munit.FunSuite:

  private def noleakage(
      losses: Array[Double],
      ceded: Array[Double],
      retained: Array[Double],
      splits: IndexedSeq[(layer: Layer, cededToLayer: Array[Double])] = IndexedSeq.empty
  ) =

    assertVecEquals(ceded + retained, losses)
    assertVecEquals(splits.map(_.cededToLayer).reduce(_ + _), ceded)
  end noleakage

  test("from retention") {
    val tower = Tower.fromRetention(5.0, Vector(6.0, 7.0, 8.0))
    assertEquals(tower.layers.length, 3)

    assertEquals(tower.layers.head.occLimit, Some(6.0))
    assertEquals(tower.layers(1).occLimit, Some(7.0))
    assertEquals(tower.layers(2).occLimit, Some(8.0))

    assertEquals(tower.layers.head.occRetention, Some(5.0))
    assertEquals(tower.layers(1).occRetention, Some(11.0))
    assertEquals(tower.layers(2).occRetention, Some(18.0))

  }

  test("Empty tower cedes nothing, including above the parallel threshold") {
    val tower = Tower(IndexedSeq.empty)
    for n <- Seq(3, 50000, 60001) do
      val iterations = Array.tabulate(n)(i => i / 3 + 1)
      val amounts = Array.tabulate(n)(i => (i % 7).toDouble + 0.5)

      val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

      assert(splits.isEmpty)
      assertVecEquals(ceded, Array.fill(n)(0.0))
      assertVecEquals(retained, amounts)
    end for
  }

  test("splitAmntFast rejects unsorted years, including above the parallel threshold") {
    val tower = Tower.fromRetention(5.0, Vector(6.0, 7.0))
    // Grouped-but-unsorted and interleaved inputs both rejected; the bad index is reported.
    for years <- Seq(Array(3, 3, 1, 1, 2), Array(1, 2, 1)) do
      intercept[IllegalArgumentException] {
        tower.splitAmntFast(years, Array.fill(years.length)(10.0))
      }
    end for

    val n = 60001
    val big = Array.tabulate(n)(i => i / 3 + 1)
    big(n - 1) = 0
    val ex = intercept[IllegalArgumentException] {
      tower.splitAmntFast(big, Array.fill(n)(10.0))
    }
    assert(ex.getMessage.contains(s"years(${n - 1})"), ex.getMessage)
  }

  test("splitAmntFast accepts negative and repeated years") {
    val tower = Tower(IndexedSeq(Layer(aggLimit = Some(15.0))))
    val years = Array(-2, -2, -1, 0, 0)
    val losses = Array(10.0, 10.0, 10.0, 10.0, 10.0)
    val (ceded, retained, splits) = tower.splitAmntFast(years, losses)
    assertVecEquals(ceded, Array(10.0, 5.0, 10.0, 10.0, 5.0))
    noleakage(losses, ceded, retained, splits)
  }

  test("One layer, one claim. Inf xs 10, loss 12.0") {
    val iterations = Array(1)
    val amounts = Array(12.0)

    val layer1 = Layer(occRetention = Some(10.0))

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    assertEqualsDouble(ceded.head, 2.0, 0.001)
    assertEqualsDouble(retained.head, 10.0, 0.001)
    assertEqualsDouble(splits.head.cededToLayer.head, 2.0, 0.001)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, one claim. 5 xs 10, loss 17.0") {
    val iterations = Array(1)
    val amounts = Array(17.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0))

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    assertEqualsDouble(ceded.head, 5.0, 0.001)
    assertEqualsDouble(retained.head, 12.0, 0.001)
    assertEqualsDouble(splits.head.cededToLayer.head, 5.0, 0.001)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, one claim. 5 xs 10 occ, share 0.5, loss 17.0") {
    val iterations = Array(1)
    val amounts = Array(17.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0), share = 0.5)

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    assertEqualsDouble(ceded.head, 2.5, 0.001)
    assertEqualsDouble(retained.head, 14.5, 0.001)
    assertEqualsDouble(splits.head.cededToLayer.head, 2.5, 0.001)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, two claims. 5 xs 10 occ, losses [14.0, 12.0] share 0.5") {
    val iterations = Array(1, 1)
    val amounts = Array(14.0, 12.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0), share = 0.5)

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val cededExpected = Array(2.0, 1.0) // (14 -10) * 0.5, (12 - 10) * 0.5
    assertVecEquals(ceded, cededExpected)
    assertVecEquals(retained, amounts - cededExpected)
    assertVecEquals(splits.head.cededToLayer, cededExpected)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, 5 claims across 3 iters. 5 xs 10 occ") {
    val iterations = Array(1, 2, 2, 3, 3)
    val amounts = Array(9.0, 14.0, 12.0, 9.0, 15.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0), share = 0.5)

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val cededExpected = Array(0.0, 2.0, 1.0, 0.0, 2.5)
    assertVecEquals(ceded, cededExpected)
    assertVecEquals(retained, amounts - cededExpected)
    assertVecEquals(splits.head.cededToLayer, cededExpected)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, 5 claims across 3 iters. 5 xs 10 fra") {
    val iterations = Array(1, 2, 2, 3, 3)
    val amounts = Array(9.0, 14.0, 12.0, 9.0, 15.0)

    val layer1 = Layer(occRetention = Some(10.0), occType = DeductibleType.Franchise, share = 0.5)

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val cededExpected = Array(0.0, 7.0, 6.0, 0.0, 7.5)
    assertVecEquals(ceded, cededExpected)
    assertVecEquals(retained, amounts - cededExpected)
    assertVecEquals(splits.head.cededToLayer, cededExpected)
    noleakage(amounts, ceded, retained, splits)
  }

  test("Occ franchise 5 xs 10: pays from x >= 10, the full loss capped at limit + franchise point") {
    // One loss per iteration, so only the occurrence terms matter
    val amounts = Array(9.0, 10.0, 12.0, 15.0, 16.0, 100.0)
    val iterations = Array.tabulate(amounts.length)(_ + 1)
    val layer = Layer(occRetention = Some(10.0), occLimit = Some(5.0), occType = DeductibleType.Franchise)

    val (ceded, retained, splits) = Tower(IndexedSeq(layer)).splitAmntFast(iterations, amounts)

    val cededExpected = Array(0.0, 10.0, 12.0, 15.0, 15.0, 15.0)
    assertVecEquals(ceded, cededExpected)
    assertVecEquals(retained, amounts - cededExpected)
    noleakage(amounts, ceded, retained, splits)
  }

  test("Agg franchise 5 xs 10: a year whose total exactly reaches the franchise point pays in full") {
    // Iteration 1 totals 4 + 6 = 10 (pays 10), iteration 2 totals 9.5 (pays 0), iteration 3 totals 40 (pays 15)
    val iterations = Array(1, 1, 2, 3, 3)
    val amounts = Array(4.0, 6.0, 9.5, 30.0, 10.0)
    val layer = Layer(aggRetention = Some(10.0), aggLimit = Some(5.0), aggType = DeductibleType.Franchise)

    val (ceded, retained, splits) = Tower(IndexedSeq(layer)).splitAmntFast(iterations, amounts)

    val cededExpected = Array(0.0, 10.0, 0.0, 15.0, 0.0)
    assertVecEquals(ceded, cededExpected)
    noleakage(amounts, ceded, retained, splits)
  }

  test("Occ franchise in splitAmntFast agrees with rpt.franchiseFunction, SIMD body and tail") {
    import rpt.*
    import Retentions.Retention
    import Limits.Limit
    val terms = Seq((Some(5.0), Some(10.0)), (None, Some(10.0)), (Some(5.0), None))
    // Lengths straddle every SIMD width, so both the vector loop and the scalar tail see boundary values
    for
      n <- Seq(1, 3, 7, 8, 9, 17, 37)
      (lim, ret) <- terms
    do
      val pattern = Array(0.0, 9.999, 10.0, 10.001, 12.0, 14.999, 15.0, 15.001, 50.0, 5.0, 4.999)
      val amounts = Array.tabulate(n)(i => pattern(i % pattern.length))
      val iterations = Array.tabulate(n)(_ + 1)
      val layer = Layer(occRetention = ret, occLimit = lim, occType = DeductibleType.Franchise)

      val (ceded, _, _) = Tower(IndexedSeq(layer)).splitAmntFast(iterations, amounts)

      val expected = amounts.clone()
      expected.franchiseFunction(lim.map(Limit(_)), ret.map(Retention(_)))
      assertVecEquals(ceded, expected)
    end for
  }

  /** Plain scalar reference for one layer, straight from the DeductibleType definitions: occurrence terms per loss,
    * aggregate terms on each year's running total, share, then back to per-loss amounts.
    */
  private def referenceSplit(layer: Layer, years: Array[Int], losses: Array[Double]): Array[Double] =
    def terms(t: DeductibleType, lim: Option[Double], ret: Option[Double])(x: Double): Double =
      val l = lim.getOrElse(Double.PositiveInfinity)
      val r = ret.getOrElse(0.0)
      t match
        case DeductibleType.Retention        => math.min(math.max(x - r, 0.0), l)
        case DeductibleType.Franchise        => if x >= r then math.min(x, l + r) else 0.0
        case DeductibleType.ReverseFranchise => math.min(if ret.forall(x <= _) then x else 0.0, l)
      end match
    end terms
    val occ = losses.map(terms(layer.occType, layer.occLimit, layer.occRetention))
    val out = new Array[Double](losses.length)
    var i = 0
    while i < losses.length do
      var running = 0.0
      var prevPaid = 0.0
      val year = years(i)
      while i < losses.length && years(i) == year do
        running += occ(i)
        val paid = terms(layer.aggType, layer.aggLimit, layer.aggRetention)(running) * layer.share
        out(i) = paid - prevPaid
        prevPaid = paid
        i += 1
      end while
    end while
    out
  end referenceSplit

  test("splitAmntFast matches a scalar reference: occ-only and agg layers, with share, sequential and parallel sizes") {
    val layers = IndexedSeq(
      Layer(occRetention = Some(10.0), occLimit = Some(20.0)), // occ only, share 1
      Layer(occRetention = Some(5.0), occLimit = Some(15.0), share = 0.35), // occ only, share
      Layer(occRetention = Some(12.0), occType = DeductibleType.Franchise, share = 0.6), // occ franchise, share
      Layer(occRetention = Some(2.0), aggRetention = Some(15.0), aggLimit = Some(40.0), share = 0.25),
      Layer(
        occLimit = Some(30.0),
        aggRetention = Some(20.0),
        aggLimit = Some(25.0),
        aggType = DeductibleType.Franchise,
        share = 0.8
      ),
      Layer(aggRetention = Some(60.0), aggType = DeductibleType.ReverseFranchise, share = 0.5),
      Layer(aggLimit = Some(50.0), share = 0.1) // agg limit only
    )
    // 60001 losses exercises the JVM's parallel branch (>= 50000 losses and > 1 layer)
    for n <- Seq(1, 13, 1000, 60001) do
      val r = scala.util.Random(n)
      val years = Array.fill(n)(1 + r.nextInt(math.max(1, n / 4))).sorted
      val losses = Array.fill(n)(r.nextDouble() * 40.0)

      val (ceded, retained, splits) = Tower(layers).splitAmntFast(years, losses)

      for (layer, col) <- splits do assertVecEquals(col, referenceSplit(layer, years, losses))
      end for
      noleakage(losses, ceded, retained, splits)
    end for
  }

  test("Agg - One layer, 5 claims across 3 iters. 3 xs 10 occ, 3 xs 1.5 agg") {
    val iterations = Array(1, 2, 2, 3, 3)
    val amounts = Array(12.0, 14.0, 12.0, 11.5, 10.5)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(3.0), aggLimit = Some(3.0), aggRetention = Some(1.5))

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val cededExpected = Array(0.5, 1.5, 1.5, 0.0, 0.5)
    assertVecEquals(ceded, cededExpected)
    assertVecEquals(retained, amounts - cededExpected)
    assertVecEquals(splits.head.cededToLayer, cededExpected)
    noleakage(amounts, ceded, retained, splits)
  }

  test("throws if inputs do not match dimensions") {
    val tower = Tower.fromRetention(5.0, Vector(5.0, 5.0, 5.0))

    intercept[AssertionError] {
      tower.splitAmntFast(
        Array[Int](1),
        Array[Double](1.0, 2.0)
      )
    }

    intercept[AssertionError] {
      tower.splitAmntFast(
        Array[Int](1, 2),
        Array[Double](1.0)
      )
    }

  }

  test("Multple layers") {
    val tower = Tower.fromRetention(5.0, Vector(5.0, 5.0, 5.0))
    val iterations = Array(1, 1, 2, 3)
    val amounts = Array(7.0, 14.0, 21.0, 4)

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val l1 = splits.head.cededToLayer
    val l2 = splits(1).cededToLayer
    val l3 = splits.last.cededToLayer

    assertVecEquals(ceded, l1 + l2 + l3)
    noleakage(amounts, ceded, retained, splits)
    assertVecEquals(l1, Array(2.0, 5, 5, 0))
    assertVecEquals(l2, Array(0.0, 4, 5, 0))
    assertVecEquals(l3, Array(0.0, 0.0, 5, 0))

  }

  test("Multple layers, iterations") {
    val tower = Tower.fromRetention(5.0, Vector(5.0, 5.0, 5.0))
    val iterations = Array(1, 1, 2, 3)
    val amounts = Array(7.0, 14.0, 21.0, 4)

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val l1 = splits.head.cededToLayer
    val l2 = splits(1).cededToLayer
    val l3 = splits.last.cededToLayer

    assertVecEquals(ceded, l1 + l2 + l3)
    noleakage(amounts, ceded, retained, splits)
    assertVecEquals(l1, Array(2.0, 5, 5, 0))
    assertVecEquals(l2, Array(0.0, 4, 5, 0))
    assertVecEquals(l3, Array(0.0, 0.0, 5, 0))

  }

  test("Multple layers, 1@100") {
    val tower = Tower.oneAt100(5.0, Vector(5.0, 5.0, 5.0))
    val iterations = Array(1, 1, 1, 2)
    val amounts = Array(12.0, 9.0, 16.0, 7)

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    val l1 = splits.head.cededToLayer
    val l2 = splits(1).cededToLayer
    val l3 = splits.last.cededToLayer

    assertVecEquals(ceded, l1 + l2 + l3)
    noleakage(amounts, ceded, retained, splits)
    assertVecEquals(l1, Array(5.0, 4, 1, 2)) // blows up agg limit in year 1
    assertVecEquals(l2, Array(2.0, 0, 5, 0))
    assertVecEquals(l3, Array(0.0, 0.0, 1, 0))

  }

end TowerSuite
