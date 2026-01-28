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

// ./mill benchmark.runJmh "vecxt.benchmark.VarianceBenchmark" -jvmArgs --add-modules=jdk.incubator.vector -rf json -wi 2 -i 3 -f 1

/** 231] Benchmark (len) Mode Cnt Score Error Units 231] VarianceBenchmark.var_simd_twopass 1000 thrpt 3 1087302.435 ±
  * 16013.286 ops/s 231] VarianceBenchmark.var_simd_twopass 100000 thrpt 3 9578.869 ± 334.606 ops/s 231]
  * VarianceBenchmark.var_simd_welford 1000 thrpt 3 436244.559 ± 6158.585 ops/s 231] VarianceBenchmark.var_simd_welford
  * 100000 thrpt 3 4187.715 ± 203.266 ops/s
  */

@State(Scope.Thread)
class VarianceBenchmark extends BLASBenchmark:

  @Param(Array("1000", "100000"))
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

  // @Benchmark
  // def var_naive_twopass(bh: Blackhole) =
  //   val r = arr.variance2
  //   bh.consume(r);
  // end var_naive_twopass

  @Benchmark
  def var_simd_twopass(bh: Blackhole) =
    val r = arr.meanAndVarianceTwoPass(VarianceMode.Sample).variance
    bh.consume(r);
  end var_simd_twopass

  // @Benchmark
  // def var_simd_welford(bh: Blackhole) =
  //   val r = arr.meanAndVarianceWelfordSIMD(VarianceMode.Sample).variance
  //   bh.consume(r);
  // end var_simd_welford

  // @Benchmark
  // def var_default(bh: Blackhole) =
  //   val r = arr.variance(VarianceMode.Sample)
  //   bh.consume(r);
  // end var_default
end VarianceBenchmark


