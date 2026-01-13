package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import jdk.incubator.vector.DoubleVector
import vecxt.all.*
import scala.compiletime.uninitialized

// format: off
/**
 * Benchmark                      (n)   Mode  Cnt          Score           Error  Units
AddBenchmark.vecxt_add          10  thrpt    3  233611146.871 ± 125859919.324  ops/s
AddBenchmark.vecxt_add        1000  thrpt    3    7928734.492 ±  11559652.319  ops/s
AddBenchmark.vecxt_add      100000  thrpt    3      74348.984 ±      1738.698  ops/s
AddBenchmark.vecxt_add_vec      10  thrpt    3  285091547.069 ±   7669626.218  ops/s
AddBenchmark.vecxt_add_vec    1000  thrpt    3    6512670.752 ±    780742.072  ops/s
AddBenchmark.vecxt_add_vec  100000  thrpt    3      74825.602 ±      3966.400  ops/s
  */

@State(Scope.Thread)
class AddScalarBenchmark extends BLASBenchmark:

  final val species = DoubleVector.SPECIES_PREFERRED
  final val l = species.length()


  @Param(Array("10", "1000", "100000"))
  var n: java.lang.String = uninitialized

  var vec: Array[Double] = uninitialized
  var vec2: Array[Double] = uninitialized

  // format: off
  @Setup(Level.Trial)
  def setup: Unit =
    vec = randomDoubleArray(n.toInt);
    vec2 = randomDoubleArray(n.toInt);
    ()

  end setup

  extension (vec: Array[Double])
    inline def scalarPlusVec(d: Double): Unit =
      var i: Int = 0
      val toAdd = DoubleVector.broadcast(species, d)
      while i < species.loopBound(vec.length) do
        DoubleVector.fromArray(species, vec, i).add(toAdd).intoArray(vec, i)
        i += l
      end while

      while i < vec.length do
        vec(i) += d
        i += 1
      end while
    end scalarPlusVec


  end extension

  @Benchmark
  def vecxt_add(bh: Blackhole) =
    vec +:+= (4.5)
    bh.consume(vec);
  end vecxt_add

  @Benchmark
  def vecxt_add_vec(bh: Blackhole) =
    vec2.scalarPlusVec(4.5)
    bh.consume(vec2);
  end vecxt_add_vec
end AddScalarBenchmark

