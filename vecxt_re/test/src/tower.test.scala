package vecxt_re

import vecxt_re.*
import vecxt.all.*
import vecxt.all.given
import vecxt_re.SplitLosses.*

class TowerSuite extends munit.FunSuite:

  private def noleakage(
      losses: Array[Double],
      ceded: Array[Double],
      retained: Array[Double],
      splits: IndexedSeq[(Layer, Array[Double])] = IndexedSeq.empty
  ) =
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    assertVecEquals(ceded + retained, losses)
    assertVecEquals(splits.map(_._2).reduce(_ + _), ceded)
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

  test("One layer, one claim. Inf xs 10, loss 12.0") {
    val iterations = Array(1)
    val amounts = Array(12.0)

    val layer1 = Layer(occRetention = Some(10.0))

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, amounts)

    assertEqualsDouble(ceded.head, 2.0, 0.001)
    assertEqualsDouble(retained.head, 10.0, 0.001)
    assertEqualsDouble(splits.head._2.head, 2.0, 0.001)
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
    assertEqualsDouble(splits.head._2.head, 5.0, 0.001)
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
    assertEqualsDouble(splits.head._2.head, 2.5, 0.001)
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
    assertVecEquals(splits.head._2, cededExpected)
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
    assertVecEquals(splits.head._2, cededExpected)
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
    assertVecEquals(splits.head._2, cededExpected)
    noleakage(amounts, ceded, retained, splits)
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
    assertVecEquals(splits.head._2, cededExpected)
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

    val l1 = splits.head._2
    val l2 = splits(1)._2
    val l3 = splits.last._2

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

    val l1 = splits.head._2
    val l2 = splits(1)._2
    val l3 = splits.last._2

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

    val l1 = splits.head._2
    val l2 = splits(1)._2
    val l3 = splits.last._2

    assertVecEquals(ceded, l1 + l2 + l3)
    noleakage(amounts, ceded, retained, splits)
    assertVecEquals(l1, Array(5.0, 4, 1, 2)) // blows up agg limit in year 1
    assertVecEquals(l2, Array(2.0, 0, 5, 0))
    assertVecEquals(l3, Array(0.0, 0.0, 1, 0))

  }

end TowerSuite
