package vecxtensions

import vecxt.reinsurance.*
import vecxt.all.*
import vecxt.all.given

class TowerSuite extends munit.FunSuite:

  test("basic group functions test") {
    val groups = Array(1, 1, 1, 2, 2, 3)
    val values = Array(3.0, 8.0, 30.0, 15.5, 15.5, 30.0)
    
    // Test groupSum
    val (uniqueGroups, sums) = groupSum(groups, values)
    assertEquals(uniqueGroups.toSeq, Seq(1, 2, 3))
    assertEqualsDouble(sums(0), 41.0, 0.001) // 3 + 8 + 30 = 41
    assertEqualsDouble(sums(1), 31.0, 0.001) // 15.5 + 15.5 = 31  
    assertEqualsDouble(sums(2), 30.0, 0.001) // 30
    
    // Test groupCumSum
    val cumSums = groupCumSum(groups, values)
    val expected = Array(3.0, 11.0, 41.0, 15.5, 31.0, 30.0)
    for i <- expected.indices do
      assertEqualsDouble(cumSums(i), expected(i), 0.001, s"CumSum index $i")
    end for
    
    // Test groupDiff  
    val diffs = groupDiff(groups, values)
    val expectedDiffs = Array(3.0, 5.0, 22.0, 15.5, 0.0, 30.0) // first in group gets own value, rest get diff from prev
    for i <- expectedDiffs.indices do
      assertEqualsDouble(diffs(i), expectedDiffs(i), 0.001, s"Diff index $i: expected ${expectedDiffs(i)}, got ${diffs(i)}")
    end for
  }

  test("Tower splitAmnt - testSimple equivalent") {
    // Test data from MATLAB testSimple
    val iterations = Array(1, 1, 1, 2, 2, 3)
    val days = Array(1, 2, 3, 1, 2, 1)
    val amounts = Array(3.0, 8.0, 30.0, 15.5, 15.5, 30.0)
    
    // Create layers matching MATLAB test
    // deductibleLayers = layers([3.5, 15], [5, 0.5])
    // aggLayers = layers([7, 15], [3.5, 12])
    val layer1 = Layer(
      occLimit = Some(3.5),
      occRetention = Some(5.0),
      aggLimit = Some(7.0),
      aggRetention = Some(3.5)
    )
    val layer2 = Layer(
      occLimit = Some(15.0),
      occRetention = Some(0.5),
      aggLimit = Some(15.0),
      aggRetention = Some(12.0)
    )
    
    val tower = Tower(layers = Seq(layer1, layer2))
    
    // Split amounts
    val (ceded, retained) = tower.splitAmnt(iterations, days, amounts)
    
    // Verify ceded for first layer matches MATLAB expectation: [0,0,3,0,3.5,0]
    val expectedCeded1 = Array(0.0, 0.0, 3.0, 0.0, 3.5, 0.0)
    for i <- expectedCeded1.indices do
      assertEqualsDouble(ceded(i, 0), expectedCeded1(i), 0.001, s"Ceded layer 1, index $i: expected ${expectedCeded1(i)}, got ${ceded(i, 0)}")
    end for
    
    // Verify no leakage - total ceded + retained should equal total amounts
    val totalCeded = (0 until ceded.rows).map(i => (0 until ceded.cols).map(j => ceded(i, j)).sum).sum
    val totalRetained = retained.sum
    val totalAmounts = amounts.sum
    assertEqualsDouble(totalCeded + totalRetained, totalAmounts, 0.001, "No leakage check")
    
    // Verify each claim sums correctly
    for i <- amounts.indices do
      val claimCeded = (0 until ceded.cols).map(j => ceded(i, j)).sum
      assertEqualsDouble(claimCeded + retained(i), amounts(i), 0.001, s"Claim $i sum check")
    end for
    
    println(s"Ceded layer 1: ${(0 until ceded.rows).map(i => ceded(i, 0)).mkString("[", ",", "]")}")
    println(s"Ceded layer 2: ${(0 until ceded.rows).map(i => ceded(i, 1)).mkString("[", ",", "]")}")
    println(s"Retained: ${retained.mkString("[", ",", "]")}")
  }

  test("Tower empty input handling") {
    val tower = Tower(layers = Seq(Layer()))
    val (ceded, retained) = tower.splitAmnt(Array.empty, Array.empty, Array.empty)
    
    assertEquals(ceded.rows, 0)
    assertEquals(retained.length, 0)
  }

  test("Tower splitAmntFast vs splitAmnt - performance equivalent") {
    // Setup same data as testSimple
    val years = Array(2024, 2024, 2024, 2025, 2025, 2026)
    val days = Array(1, 100, 200, 50, 150, 75) 
    val losses = Array(3.0, 8.0, 30.0, 15.5, 15.5, 30.0)
    
    val tower = Tower(
      layers = Seq(
        Layer(
          occLimit = Some(25.0),
          occRetention = Some(10.0), 
          aggLimit = Some(50.0),
          aggRetention = Some(5.0),
          share = 0.5,
          occType = DeductibleType.Retention,
          aggType = DeductibleType.Retention
        ),
        Layer(
          occLimit = Some(40.0),
          occRetention = Some(25.0),
          aggLimit = Some(100.0), 
          aggRetention = Some(50.0),
          share = 1.0,
          occType = DeductibleType.Retention,
          aggType = DeductibleType.Retention  
        )
      )
    )
    
    // Run both implementations
    val (cededOriginal, retainedOriginal) = tower.splitAmnt(years, days, losses)
    val (cededFast, retainedFast) = tower.splitAmntFast(years, days, losses)
    
    // Check if raw matrices are identical (this should be true)
    for i <- cededOriginal.raw.indices do
      assertEqualsDouble(cededOriginal.raw(i), cededFast.raw(i), 1e-10, s"Raw data[$i] differs")
    end for
    
    // The matrices should be identical
    assertEquals(cededOriginal.rows, cededFast.rows)
    assertEquals(cededOriginal.cols, cededFast.cols)
    
    for i <- 0 until cededOriginal.rows do
      for j <- 0 until cededOriginal.cols do
        assertEqualsDouble(cededOriginal(i, j), cededFast(i, j), 1e-10, s"Ceded[$i,$j] differs")
      end for
    end for
    
    assertEquals(retainedOriginal.length, retainedFast.length)
    for i <- retainedOriginal.indices do
      assertEqualsDouble(retainedOriginal(i), retainedFast(i), 1e-10, s"Retained[$i] differs")
    end for
    
    println("Performance implementations match exactly!")
    println(s"Fast Ceded layer 1: ${(0 until cededFast.rows).map(i => cededFast(i, 0)).mkString("[", ",", "]")}")
    println(s"Fast Ceded layer 2: ${(0 until cededFast.rows).map(i => cededFast(i, 1)).mkString("[", ",", "]")}")
    println(s"Fast Retained: ${retainedFast.mkString("[", ",", "]")}")
  }

end TowerSuite