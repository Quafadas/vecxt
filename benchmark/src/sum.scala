package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
// import vecxt.Matrix.*

import scala.compiletime.uninitialized
import vecxt.all.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector

/** 2026-07-28 github devcontainer
  *
  * SumBenchmark.sum_loop 3 thrpt 3 509392485.880 ± 28476794.438 ops/s SumBenchmark.sum_loop 100 thrpt 3 22388541.489 ±
  * 1070438.908 ops/s SumBenchmark.sum_loop 100000 thrpt 3 10597.404 ± 81.607 ops/s SumBenchmark.sum_vec_alt 3 thrpt 3
  * 453626173.485 ± 643778788.723 ops/s SumBenchmark.sum_vec_alt 100 thrpt 3 106150826.931 ± 72101397.367 ops/s
  * SumBenchmark.sum_vec_alt 100000 thrpt 3 42300.013 ± 493.449 ops/s
  */

// ./mill benchmark.runJmh vecxt.benchmark.SumBenchmark -jvmArgs --add-modules=jdk.incubator.vector -rf json
@State(Scope.Thread)
class SumBenchmark extends BLASBenchmark:

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

    /** 
     * Left packing it hard in SIMD land. This is not a good strategy - does not benchmark well.
     */
    inline def sum2 =
      var sum: Double = 0.0
      var i: Int = 0
      val l = spd.length()

      while i < spd.loopBound(vec.length) do
        sum = sum + DoubleVector.fromArray(spd, vec, i).reduceLanes(VectorOperators.ADD)
        i += l
      end while
      while i < vec.length do
        sum += vec(i)
        i += 1
      end while
      sum
    end sum2

    inline def sum3 =
      var sum: Double = 0.0
      var i: Int = 0
      while i < vec.length do
        sum = sum + vec(i)
        i = i + 1
      end while
      sum
    end sum3

  end extension



  @Benchmark
  def sum_loop(bh: Blackhole) =
    val r = arr.sum3
    bh.consume(r);
  end sum_loop

  // @Benchmark
  // def sum_vec(bh: Blackhole) =
  //   val r = arr.sum2
  //   bh.consume(r);
  // end sum_vec

  @Benchmark
  def sum_vec_alt(bh: Blackhole) =
    val r = arr.sumSIMD
    bh.consume(r);
  end sum_vec_alt


end SumBenchmark
