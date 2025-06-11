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
