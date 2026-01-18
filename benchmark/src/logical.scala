package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
// import vecxt.Matrix.*
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import vecxt.all.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector

@State(Scope.Thread)
class LogicalBenchmark extends BLASBenchmark:

  @Param(Array("3", "128", "100000"))
  var len: String = uninitialized;

  var arr: Array[Double] = uninitialized


  // format: off
  @Setup(Level.Trial)
  def setup: Unit =
    arr = randomDoubleArray(len.toInt);
    ()
  end setup

  extension (vec: Array[Double])
    inline def lte2(num: Double) =
      val idx: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < vec.length do
        idx(i) = vec(i) <= num
        i += 1
      end while
      idx
  end extension

  @Benchmark
  def lte_vec(bh: Blackhole) =
    val r = arr <= 4.0
    bh.consume(r);
  end lte_vec



  @Benchmark
  def lte_loop(bh: Blackhole) =
    val r = arr.lte2(4.0)
    bh.consume(r);
  end lte_loop

end LogicalBenchmark

