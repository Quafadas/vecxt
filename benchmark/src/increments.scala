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
class IncrementBenchmark extends BLASBenchmark:

  @Param(Array("3", "100", "100000"))
  var len: String = uninitialized;

  var arr: Array[Double] = uninitialized

  // format: off
  @Setup(Level.Trial)
  def setup: Unit =
    arr = randomDoubleArray(len.toInt);
    ()
  end setup

  extension (vec: Array[Double])
    inline def increments_loop: Array[Double] =
      val out = new Array[Double](vec.length)
      out(0) = vec(0)
      var i = 1
      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out
    end increments_loop

  end extension

  @Benchmark
  def increment_normal(bh: Blackhole) =
    val r = arr.increments_loop
    bh.consume(r);
  end increment_normal

  @Benchmark
  def increment_vec(bh: Blackhole) =
    val r = arr.increments
    bh.consume(r);
  end increment_vec

end IncrementBenchmark
