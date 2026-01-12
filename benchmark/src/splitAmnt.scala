/*
 * Benchmark for Tower splitAmnt methods comparing original vs high-performance SIMD implementation
 */

package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import scala.compiletime.uninitialized
import vecxt.all.*
import vecxt.all.given
import vecxt.reinsurance.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector
import java.util.Random

@State(Scope.Thread)
class SplitAmntBenchmark extends BLASBenchmark:

  @Param(Array("10000", "1000000"))
  var len: String = uninitialized

  var years: Array[Int] = uninitialized
  var days: Array[Int] = uninitialized 
  var losses: Array[Double] = uninitialized
  var yearsIArray: IArray[Int] = uninitialized
  var daysIArray: IArray[Int] = uninitialized
  var lossesIArray: IArray[Double] = uninitialized
  var tower: Tower = uninitialized

  @Setup(Level.Trial)
  def setup: Unit =
    val size = len.toInt
    val random = new Random(42) // Fixed seed for reproducibility
    
    // Generate realistic test data
    years = Array.fill(size)(random.nextInt(size)).sorted
    days = Array.fill(size)(1 + random.nextInt(365))   // 1-365
    losses = Array.fill(size)(random.nextDouble() * 100.0) // 0-100 losses
    
    // Create IArray versions for splitAmnt2
    yearsIArray = IArray.unsafeFromArray(years)
    daysIArray = IArray.unsafeFromArray(days)
    lossesIArray = IArray.unsafeFromArray(losses)
    
    // Create realistic tower with multiple layers
    tower = Tower(
      layers = Seq(
        Layer(
          occLimit = Some(25.0),
          occRetention = Some(10.0), 
          aggLimit = Some(50),
          aggRetention = None,
          share = 0.5,
          occType = DeductibleType.Retention,
          aggType = DeductibleType.Retention
        ),
        Layer(
          occLimit = Some(40.0),
          occRetention = Some(35.0),
          aggLimit = Some(80.0), 
          aggRetention = None,
          share = 1.0,
          occType = DeductibleType.Retention,
          aggType = DeductibleType.Retention  
        ),
        Layer(
          occLimit = Some(25.0),
          occRetention = Some(70.0),
          aggLimit = Some(50.0), 
          aggRetention = Some(1.0),
          share = 0.75,
          occType = DeductibleType.Retention,
          aggType = DeductibleType.Retention  
        )
      )
    )
    

    try
      val (cededOrig: Matrix[Double], retainedOrig) = tower.splitAmnt(years, days, losses)
      val (cededFast: Matrix[Double], retainedFast) = tower.splitAmntFast(years, days, losses)
      val (ceded2Flat: Array[Double], retained2, splits2) = tower.splitAmnt2(yearsIArray, daysIArray, lossesIArray)
      val (ceded3Flat: Array[Double], retained3, splits3) = tower.splitAmnt3(yearsIArray, daysIArray, lossesIArray)
      
      println(s"✅ All four implementations work for ${size} losses")
      val (x,y) = cededOrig.shape
      
      // Verify original vs fast
      for (i <- 0 until x) do
        for (j <- 0 until y) do
          if (math.abs(cededOrig(i,j) - cededFast(i,j)) >= 1e-10) then
            println("Original vs Fast: structures failed to agree")
            println(losses(i))
            println(cededOrig.row(i).printArr)
            println(cededFast.row(i).printArr)
            assert(math.abs(cededOrig(i,j) - cededFast(i,j)) < 1e-10, s"Mismatch at index ($i,$j): ${cededOrig(i,j)} != ${cededFast(i,j)}")      
      end for
      
      // Verify original vs splitAmnt2
      for (i <- 0 until x) do
        for (j <- 0 until y) do
          val flatIdx = j * x + i  // column-major indexing
          val splitValue = splits2(j)._2(i)
          if (math.abs(cededOrig(i,j) - splitValue) >= 1e-10) then
            println("Original vs splitAmnt2: structures failed to agree")
            println(s"At ($i,$j): orig=${cededOrig(i,j)}, split2=${splitValue}")
            assert(math.abs(cededOrig(i,j) - splitValue) < 1e-10, s"Mismatch at index ($i,$j)")
      end for
      
      // Verify original vs splitAmnt3
      for (i <- 0 until x) do
        for (j <- 0 until y) do
          val splitValue = splits3(j)._2(i)
          if (math.abs(cededOrig(i,j) - splitValue) >= 1e-10) then
            println("Original vs splitAmnt3: structures failed to agree")
            println(s"At ($i,$j): orig=${cededOrig(i,j)}, split3=${splitValue}")
            assert(math.abs(cededOrig(i,j) - splitValue) < 1e-10, s"Mismatch at index ($i,$j)")
      end for
      
      // Verify retained arrays match
      for i <- 0 until x do
        assert(math.abs(retainedOrig(i) - retained2(i)) < 1e-10, s"Retained2 mismatch at $i")
        assert(math.abs(retainedOrig(i) - retained3(i)) < 1e-10, s"Retained3 mismatch at $i")
      end for
      
    catch
      case e: Exception =>
        println(s"❌ Error with ${size} losses: ${e.getMessage}")
        e.printStackTrace()
        throw e
  end setup

  @Benchmark
  def splitAmntOriginal(bh: Blackhole): Unit =
    val (ceded, retained) = tower.splitAmnt(years, days, losses)
    bh.consume(ceded)
    bh.consume(retained)
  end splitAmntOriginal

  @Benchmark  
  def splitAmntFast(bh: Blackhole): Unit =
    val (ceded, retained) = tower.splitAmntFast(years, days, losses)
    bh.consume(ceded)
    bh.consume(retained)
  end splitAmntFast

  @Benchmark
  def splitAmnt2(bh: Blackhole): Unit =
    val (cededFlat, retained, splits) = tower.splitAmnt2(yearsIArray, daysIArray, lossesIArray)
    bh.consume(cededFlat)
    bh.consume(retained)
    bh.consume(splits)
  end splitAmnt2

  @Benchmark
  def splitAmnt3(bh: Blackhole): Unit =
    val (cededFlat, retained, splits) = tower.splitAmnt3(yearsIArray, daysIArray, lossesIArray)
    bh.consume(cededFlat)
    bh.consume(retained)
    bh.consume(splits)
  end splitAmnt3

end SplitAmntBenchmark