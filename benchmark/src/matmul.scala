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
import vecxt.all.*
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import BoundsCheck.DoBoundsCheck.no

@State(Scope.Thread)
class DgemmBenchmark extends BLASBenchmark:

  var m, n, k: Int = uninitialized
  var transa: String = uninitialized
  var transb: String = uninitialized
  var alpha: Double = uninitialized
  var a: Array[Double] = uninitialized
  var lda: Int = uninitialized
  var b: Array[Double] = uninitialized
  var ldb: Int = uninitialized
  var beta: Double = uninitialized
  var c, cclone: Array[Double] = uninitialized
  var ldc: Int = uninitialized

  var matA: Matrix[Double] = uninitialized
  var matB: Matrix[Double] = uninitialized

  // format: off
  @Setup(Level.Trial)
  def setup: Unit =
    transa = "N"
    transb = "N"
    m = 100
    n = 100
    k = 100
    alpha = randomDouble();
    a = randomDoubleArray(k * m);
    b = randomDoubleArray(k * n);

    beta = randomDouble();
    // c = randomDoubleArray(m * n);
    matA = Matrix(a, (m, k))(using BoundsCheck.DoBoundsCheck.no)
    matB = Matrix(b, (k, n))(using BoundsCheck.DoBoundsCheck.no)
    ()

  end setup

  @Benchmark
  def java_dgemm(bh: Blackhole) =
    val cclone = Array.fill[Double](m*n)(0)
    blas.dgemm(
      transa,
      transb,
      m,
      n,
      k,
      alpha,
      a,
      if transa.equals("N") then m else k,
      b,
      if transb.equals("N") then k else n,
      beta,
      cclone,
      m
    );
    bh.consume(cclone);
  end java_dgemm


  @Benchmark
  def vecxt_mmult(bh: Blackhole)=
    val cclone = matA @@ matB
    bh.consume(cclone);
  end vecxt_mmult



end DgemmBenchmark
