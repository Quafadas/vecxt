package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import vecxt.arrays.*
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import vecxt.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector

@State(Scope.Thread)
class VarianceBenchmark extends BLASBenchmark:

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
    inline def variance2: Double =
      // https://www.cuemath.com/sample-variance-formula/
      val μ = vec.mean
      vec.map(i => (i - μ) * (i - μ)).sumSIMD / (vec.length - 1)
  end extension



  @Benchmark
  def var_loop(bh: Blackhole) =
    val r = arr.variance2
    bh.consume(r);
  end var_loop


  @Benchmark
  def var_vec(bh: Blackhole) =
    val r = arr.variance
    bh.consume(r);
  end var_vec
end VarianceBenchmark


