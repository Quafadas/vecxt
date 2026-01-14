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
import vecxt.reinsurance.SplitLosses.splitAmntFast

// mill benchmark.runJmh vecxt.benchmark.SplitAmntBenchmark -jvmArgs --add-modules=jdk.incubator.vector -rf json
@State(Scope.Thread)
class SplitAmntBenchmark extends BLASBenchmark:

  @Param(Array("10000", "1000000"))
  var len: String = uninitialized

  var years: Array[Int] = uninitialized
  var losses: Array[Double] = uninitialized
  var tower: Tower = uninitialized

  @Setup(Level.Trial)
  def setup: Unit =
    val size = len.toInt
    val random = new Random(42) // Fixed seed for reproducibility

    // Generate realistic test data
    years = Array.fill(size)(random.nextInt(size)).sorted
    losses = Array.fill(size)(random.nextDouble() * 100.0) // 0-100 losses

    // Create realistic tower with multiple layers
    tower = Tower.singleShot(10.0, IndexedSeq(25.0, 40.0, 25.0, 10.0))

  end setup

  @Benchmark
  def splitAmntFast(bh: Blackhole): Unit =
    val (cededTotals, retained, splits) = tower.splitAmntFast(years, losses)
    bh.consume(cededTotals)
    bh.consume(retained)
    bh.consume(splits)
  end splitAmntFast

end SplitAmntBenchmark

// mill benchmark.runJmh vecxt.benchmark.SplitAmntBenchmark -jvmArgs "--add-modules=jdk.incubator.vector -Xms2G -Xmx4G -Xlog:gc*:file=gc.log:time,level -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints -XX:StartFlightRecording=filename=recording.jfr,duration=120s,settings=profile" -prof gc,stack -wi 5 -i 10 -f 1 -rf json