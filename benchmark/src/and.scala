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
class AndBooleanBenchmark extends BLASBenchmark:

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
    inline def and2(thatIdx: Array[Boolean]) =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < vec.length do
        result(i) = vec(i) && thatIdx(i)
        i += 1
      end while
      result
  end extension

  @Benchmark
  def and_loop(bh: Blackhole) =
    val r = arr.and2(arr2)
    bh.consume(r);
  end and_loop


  @Benchmark
  def and_loop_vec(bh: Blackhole) =
    val r = arr && arr2
    bh.consume(r);
  end and_loop_vec

end AndBooleanBenchmark



