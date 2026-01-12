package vecxtensions

import vecxt.reinsurance.*
import vecxt.all.*
import vecxt.all.given

class TowerSuite extends munit.FunSuite:

  
  
  private def assertVecEquals(v1: Array[Double], v2: Array[Double])(implicit loc: munit.Location): Unit =
    assert(v1.length == v2.length)
    var i: Int = 0;
    while i < v1.length do
      assertEqualsDouble(v1(i), v2(i), 1 / 1e6, clue = s"at index $i")
      i += 1
    end while
  end assertVecEquals

  private def noleakage(losses:Array[Double], ceded: Array[Double], retained: Array[Double], splits: IndexedSeq[(Layer, Array[Double])] = IndexedSeq.empty) = 
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    assertVecEquals(ceded + retained, losses)
    assertVecEquals(splits.map(_._2).reduce(_ + _), ceded)

  // test("Tower splitAmnt - testSimple equivalent") {    
  //   val iterations = Array(1, 1, 1, 2, 2, 3)
  //   val days = Array(1, 2, 3, 1, 2, 1)
  //   val amounts = Array(3.0, 8.0, 30.0, 15.5, 15.5, 30.0)
    
  //   // Create layers matching MATLAB test
  //   // deductibleLayers = layers([3.5, 15], [5, 0.5])
  //   // aggLayers = layers([7, 15], [3.5, 12])
  //   val layer1 = Layer(
  //     occLimit = Some(3.5),
  //     occRetention = Some(5.0),
  //     aggLimit = Some(7.0),
  //     aggRetention = Some(3.5)      
  //   )
  //   val layer2 = Layer(
  //     occLimit = Some(15.0),
  //     occRetention = Some(0.5),
  //     aggLimit = Some(15.0),
  //     aggRetention = Some(12.0)
  //   )
    
  //   val tower = Tower(layers = IndexedSeq(layer1, layer2))
    
  //   // Split amounts
  //   val (ceded, retained, splits) = tower.splitAmntFast(iterations, days, amounts)
    
  //   // Verify ceded for first layer matches MATLAB expectation: [0,0,3,0,3.5,0]
  //   val expectedCeded1 = Array(0.0, 0.0, 3.0, 0.0, 3.5, 0.0)
  //   val cededL1 = splits(0)._2
  //   for i <- expectedCeded1.indices do
  //     assertEqualsDouble(expectedCeded1(i), cededL1(i), 0.001, s"Ceded layer 1, index $i: expected ${expectedCeded1(i)}, got ${cededL1(i)}")
  //   end for
    
  //       // Verify ceded for first layer matches MATLAB expectation: [0,0,3,0,3.5,0]
  //   val expectedCeded2 = Array(0.0, 0.0, 3.0, 0.0, 3.5, 0.0)
  //   val cededL1 = splits(0)._2
  //   for i <- expectedCeded1.indices do
  //     assertEqualsDouble(expectedCeded1(i), cededL1(i), 0.001, s"Ceded layer 1, index $i: expected ${expectedCeded1(i)}, got ${cededL1(i)}")
  //   end for

  //   // Verify no leakage - total ceded + retained should equal total amounts
  //   val totalCeded = (0 until ceded.length).map(i => ceded(i)).sum
  //   val totalRetained = retained.sum
  //   val totalAmounts = amounts.sum
  //   assertEqualsDouble(totalCeded + totalRetained, totalAmounts, 0.001, "No leakage check")
    
  //   // Verify each claim sums correctly
  //   for i <- amounts.indices do
  //     val claimCeded = (0 until ceded.cols).map(j => ceded(i, j)).sum
  //     assertEqualsDouble(claimCeded + retained(i), amounts(i), 0.001, s"Claim $i sum check")
  //   end for
    
  //   println(s"Ceded layer 1: ${(0 until ceded.rows).map(i => ceded(i, 0)).mkString("[", ",", "]")}")
  //   println(s"Ceded layer 2: ${(0 until ceded.rows).map(i => ceded(i, 1)).mkString("[", ",", "]")}")
  //   println(s"Retained: ${retained.mkString("[", ",", "]")}")
  // }

  test("Tower empty input handling") {
    val tower = Tower(layers = IndexedSeq(Layer()))
    val (ceded, retained) = tower.splitAmnt(Array.empty, Array.empty, Array.empty)
    
    assertEquals(ceded.rows, 0)
    assertEquals(retained.length, 0)
    
  }

  test("One layer, one claim. Inf xs 10, loss 12.0") {
    val iterations = Array(1)
    val days = Array(1)
    val amounts = Array(12.0)

    val layer1 = Layer(occRetention = Some(10.0))

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, days, amounts)

    assertEqualsDouble(ceded.head, 2.0, 0.001)
    assertEqualsDouble(retained.head, 10.0, 0.001)
    assertEqualsDouble(splits.head._2.head, 2.0, 0.001)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, one claim. 5 xs 10, loss 17.0") {
    val iterations = Array(1)
    val days = Array(1)
    val amounts = Array(17.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0))

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, days, amounts)

    assertEqualsDouble(ceded.head, 5.0, 0.001)
    assertEqualsDouble(retained.head, 12.0, 0.001)
    assertEqualsDouble(splits.head._2.head, 5.0, 0.001)
    noleakage(amounts, ceded, retained, splits)
  }

  test("One layer, one claim. 5 xs 10 occ, share 0.5, loss 17.0") {
    val iterations = Array(1)
    val days = Array(1)
    val amounts = Array(17.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0), share = 0.5)

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, days, amounts)

    assertEqualsDouble(ceded.head, 2.5, 0.001)
    assertEqualsDouble(retained.head, 14.5, 0.001)
    assertEqualsDouble(splits.head._2.head, 2.5, 0.001)
    noleakage(amounts, ceded, retained, splits)
  }  

  test("One layer, two claims. 5 xs 10 occ, losses [14.0, 12.0] share 0.5") {
    val iterations = Array(1, 1)
    val days = Array(1, 2)
    val amounts = Array(14.0, 12.0)

    val layer1 = Layer(occRetention = Some(10.0), occLimit = Some(5.0), share = 0.5)

    val tower = Tower(IndexedSeq(layer1))

    val (ceded, retained, splits) = tower.splitAmntFast(iterations, days, amounts)

    val cededExpected = Array(2.0, 1.0) // (14 -10) * 0.5
    assertVecEquals(ceded, cededExpected)
    assertVecEquals(retained, amounts - cededExpected)
    assertVecEquals(splits.head._2, cededExpected)
    noleakage(amounts, ceded, retained, splits)    
  }


end TowerSuite