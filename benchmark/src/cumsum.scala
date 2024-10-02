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
// import vecxt.Matrix.*
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import vecxt.all.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector

@State(Scope.Thread)
class CumsumBenchmark extends BLASBenchmark:

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
      def cumsum2: Unit = 
        var i = 1
        while i < vec.length do
          vec(i) = vec(i - 1) + vec(i)
          i = i + 1
        end while        
      end cumsum2

  end extension



  @Benchmark
  def cumsum_loop(bh: Blackhole) =
    val r = arr.cumsum2
    bh.consume(r);
  end cumsum_loop

  @Benchmark
  def cumsum_vec(bh: Blackhole) =
    val r = arr.cumsum
    bh.consume(r);
  end cumsum_vec

end CumsumBenchmark
