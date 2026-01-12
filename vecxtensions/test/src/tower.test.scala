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

  test("Tower splitAmnt2 with IArray API") {
    // Test data
    val years = IArray(2024, 2024, 2024, 2025, 2025, 2026)
    val days = IArray(1, 100, 200, 50, 150, 75) 
    val losses = IArray(3.0, 8.0, 30.0, 15.5, 15.5, 30.0)
    
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
    
    // Run new implementation
    val (cededFlat, retained2, splits) = tower.splitAmnt2(years, days, losses)
    
    // Run reference implementation for comparison
    val (cededMatrix, retainedRef) = tower.splitAmnt(years.asInstanceOf[Array[Int]], days.asInstanceOf[Array[Int]], losses.asInstanceOf[Array[Double]])
    
    // Verify splits structure
    assertEquals(splits.length, 2, "Should have 2 layer splits")
    assertEquals(splits(0)._1, tower.layers(0), "First split should reference first layer")
    assertEquals(splits(1)._1, tower.layers(1), "Second split should reference second layer")
    
    // Verify per-layer arrays
    for layerIdx <- 0 until 2 do
      val (layer, layerData) = splits(layerIdx)
      assertEquals(layerData.length, losses.length, s"Layer $layerIdx data length")
      
      // Compare with reference implementation
      for i <- 0 until losses.length do
        assertEqualsDouble(layerData(i), cededMatrix(i, layerIdx), 1e-10, s"Layer $layerIdx, loss $i")
      end for
    end for
    
    // Verify flat ceded array (column-major order)
    for layerIdx <- 0 until 2 do
      for i <- 0 until losses.length do
        val flatIdx = layerIdx * losses.length + i
        assertEqualsDouble(cededFlat(flatIdx), cededMatrix(i, layerIdx), 1e-10, s"Flat ceded[$flatIdx]")
      end for
    end for
    
    // Verify retained matches reference
    assertEquals(retained2.length, retainedRef.length)
    for i <- retained2.indices do
      assertEqualsDouble(retained2(i), retainedRef(i), 1e-10, s"Retained[$i]")
    end for
    
    // Verify no leakage - total ceded + retained should equal total losses
    val totalCeded = cededFlat.sum
    val totalRetained = retained2.sum
    val totalLosses = losses.asInstanceOf[Array[Double]].sum
    assertEqualsDouble(totalCeded + totalRetained, totalLosses, 0.001, "No leakage check")
    
    println("splitAmnt2 implementation matches reference exactly!")
    println(s"Splits layer 1: ${splits(0)._2.mkString("[", ",", "]")}")
    println(s"Splits layer 2: ${splits(1)._2.mkString("[", ",", "]")}")
    println(s"Retained: ${retained2.mkString("[", ",", "]")}")
  }

end TowerSuite