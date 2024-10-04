/*
 * Copyright 2020, 2021, Ludovic Henry
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Please contact git@ludovic.dev or visit ludovic.dev if you need additional
 * information or have any questions.
 */

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
      val species = DoubleVector.SPECIES_PREFERRED
      var i: Int = 0
      val l = species.length()

      while i < species.loopBound(vec.length) do
        DoubleVector.fromArray(species, vec, i).add(DoubleVector.broadcast(species, d)).intoArray(vec, i)
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

