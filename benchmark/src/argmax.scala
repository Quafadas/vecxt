package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import jdk.incubator.vector.DoubleVector
import vecxt.all.*
import scala.compiletime.uninitialized

// format: off
/**

  */

  // mill benchmark.runJmh vecxt.benchmark.ArgmaxBenchmark -jvmArgs --add-modules=jdk.incubator.vector

@State(Scope.Thread)
class ArgmaxBenchmark extends BLASBenchmark:

  final val species = DoubleVector.SPECIES_PREFERRED
  final val l = species.length()


  @Param(Array("10", "1000", "100000"))
  var n: java.lang.String = uninitialized

  var vec: Array[Double] = uninitialized


  // format: off
  @Setup(Level.Iteration)
  def setup: Unit =
    vec = randomDoubleArray(n.toInt);

    ()

  end setup

  extension (vec: Array[Double])
    inline def argmax_scalar: Int =
      var maxIdx = 0
      var maxVal = vec(0)
      var i = 1
      while i < vec.length do
        if vec(i) > maxVal then
          maxVal = vec(i)
          maxIdx = i
        end if
        i += 1
      end while
      maxIdx

  end extension

  @Benchmark
  def vecxt_add(bh: Blackhole) =
    val am = vec.argmax_scalar
    bh.consume(vec);
  end vecxt_add

  @Benchmark
  def vecxt_add_vec(bh: Blackhole) =
    val am2 = vec.argmax
    bh.consume(am2);
  end vecxt_add_vec
end ArgmaxBenchmark
