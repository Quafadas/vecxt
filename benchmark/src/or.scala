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
class OrBooleanBenchmark extends BLASBenchmark:

  @Param(Array("3", "128", "100000"))
  var len: String = uninitialized;

  var arr: Array[Boolean] = uninitialized
  var arr2: Array[Boolean] = uninitialized

  // format: off
  @Setup(Level.Trial)
  def setup: Unit =
    arr = randomBooleanArray(len.toInt);
    arr2 = randomBooleanArray(len.toInt);
    ()
  end setup

  extension (vec: Array[Boolean])
    inline def or2(thatIdx: Array[Boolean]) =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < vec.length do
        result(i) = vec(i) || thatIdx(i)
        i += 1
      end while
      result
  end extension

  @Benchmark
  def or_loop(bh: Blackhole) =
    val r = arr.or2(arr2)
    bh.consume(r);
  end or_loop

  @Benchmark
  def or_vec(bh: Blackhole) =
    val r = arr || arr2
    bh.consume(r);
  end or_vec
end OrBooleanBenchmark
