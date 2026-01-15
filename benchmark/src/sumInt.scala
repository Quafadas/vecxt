package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
// import vecxt.Matrix.*
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import vecxt.all.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.IntVector
import dev.ludovic.netlib.blas.BLAS

@State(Scope.Thread)
class SumIntBenchmark extends BLASBenchmark:

  @Param(Array("3", "100", "100000"))
  var len: String = uninitialized;

  var arr: Array[Int] = uninitialized

  // format: off
  @Setup(Level.Trial)
  def setup: Unit =

    arr = randomIntArray(len.toInt);
    ()

  end setup



  extension (vec: Array[Int])


    inline def sum2 =
      var sum: Int = 0
      var i: Int = 0
      val l = spi.length()

      while i < spi.loopBound(vec.length) do
        sum = sum + IntVector.fromArray(spi, vec, i).reduceLanes(VectorOperators.ADD)
        i = i + l
      end while
      while i < vec.length do
        sum += vec(i)
        i = i + 1
      end while
      sum
    end sum2

    inline def sum3 =
      var sum: Int = 0
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

  @Benchmark
  def sum_vec(bh: Blackhole) =
    val r = arr.sum2
    bh.consume(r);
  end sum_vec

  @Benchmark
  def sum_vec_alt(bh: Blackhole) =
    val r = arr.sumSIMD
    bh.consume(r);
  end sum_vec_alt
end SumIntBenchmark
