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
class CountTrueBenchmark extends BLASBenchmark:

  @Param(Array("3", "128", "100000"))
  var len: String = uninitialized;

  var arr: Array[Boolean] = uninitialized

  // format: off
  @Setup(Level.Trial)
  def setup: Unit =
    arr = randomBooleanArray(len.toInt);
    ()
  end setup

  extension (vec: Array[Boolean])
    inline def countTrue2: Int =
      var sum = 0
      var i = 0
      while i < vec.length do
        if vec(i) then sum += 1
        end if
        i += 1
      end while
      sum

  end extension

  @Benchmark
  def countTrue_loop(bh: Blackhole) =
    val r = arr.countTrue2
    bh.consume(r);
  end countTrue_loop


  @Benchmark
  def countTrue_loop_vec(bh: Blackhole) =
    val r = arr.trues
    bh.consume(r);
  end countTrue_loop_vec
end CountTrueBenchmark

