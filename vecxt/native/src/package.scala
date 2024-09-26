/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package vecxt

import org.ekrich.blas.unsafe.*
import scala.scalanative.unsafe.*
import scala.util.chaining.*
import vecxt.dimCheck
import vecxt.BoundsCheck
import vecxt.Matrix.*
import org.ekrich.blas.*

export extensions.*

object extensions:

  extension (a: Matrix)
    inline def matmul(b: Matrix)(using inline boundsCheck: BoundsCheck): Matrix =
      dimMatCheck(a, b)
      val newArr = Array.ofDim[Double](a.rows * b.cols)
      // Note, might need to deal with transpose later.
      blas.cblas_dgemm(
        blasEnums.CblasColMajor,
        blasEnums.CblasNoTrans,
        blasEnums.CblasNoTrans,
        a.rows,
        b.cols,
        a.cols,
        1.0,
        a.raw.at(0),
        a.rows,
        b.raw.at(0),
        b.rows,
        1.0,
        newArr.at(0),
        a.rows
      )
      Matrix(newArr, (a.rows, b.cols))
    end matmul
  end extension

  extension (vec: Array[Boolean])
    inline def countTrue: Int =
      var sum = 0
      for i <- 0 until vec.length do if vec(i) then sum = sum + 1
      end for
      sum
    end countTrue

    inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) && thatIdx(i)
      end for
      result
    end &&

    inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) || thatIdx(i)
      end for
      result
    end ||

    // def copy: Array[Boolean] =
    //   val copyOfThisVector: Array[Boolean] = new Array[Boolean](vec.length)
    //   var i = 0
    //   while i < vec.length do
    //     copyOfThisVector(i) = vec(i)
    //     i = i + 1
    //   end while
    //   copyOfThisVector
    // end copy
  end extension

  extension (vec: Array[Double])

    def apply(index: Array[Boolean]) =
      val trues = index.countTrue
      val newVec = new Array[Double](trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j += 1
      end for
      newVec
    end apply

    def increments: Array[Double] =
      val out = new Array[Double](vec.length)
      out(0) = vec(0)
      var i = 1
      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out
    end increments

    inline def pearsonCorrelationCoefficient(thatVector: Array[Double])(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Double =
      dimCheck(vec, thatVector)
      val n = vec.length
      var i = 0

      var sum_x = 0.0
      var sum_y = 0.0
      var sum_xy = 0.0
      var sum_x2 = 0.0
      var sum_y2 = 0.0

      while i < n do
        sum_x = sum_x + vec(i)
        sum_y = sum_y + thatVector(i)
        sum_xy = sum_xy + vec(i) * thatVector(i)
        sum_x2 = sum_x2 + vec(i) * vec(i)
        sum_y2 = sum_y2 + thatVector(i) * thatVector(i)
        i = i + 1
      end while
      (n * sum_xy - (sum_x * sum_y)) / Math.sqrt(
        (sum_x2 * n - sum_x * sum_x) * (sum_y2 * n - sum_y * sum_y)
      )
    end pearsonCorrelationCoefficient

    inline def spearmansRankCorrelation(thatVector: Array[Double])(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Double =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    // An alias - pearson is the most commonly requested type of correlation
    inline def corr(thatVector: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Double =
      pearsonCorrelationCoefficient(thatVector)

    inline def elementRanks: Array[Double] =
      val indexed: Array[(Double, Int)] = vec.zipWithIndex
      indexed.sortInPlace()(Ordering.by(_._1))

      val ranks: Array[Double] = new Array[Double](vec.length)
      ranks(indexed.last._2) = vec.length
      var currentValue: Double = indexed(0)._1
      var r0: Int = 0
      var rank: Int = 1
      while rank < vec.length do
        val temp: Double = indexed(rank)._1
        val end: Int =
          if temp != currentValue then rank
          else if rank == vec.length - 1 then rank + 1
          else -1
        if end > -1 then
          val avg: Double = (1.0 + (end + r0)) / 2.0
          var i: Int = r0;
          while i < end do
            ranks(indexed(i)._2) = avg
            i += 1
          end while
          r0 = rank
          currentValue = temp
        end if
        rank += 1
      end while
      ranks
    end elementRanks

    inline def variance: Double =
      // https://www.cuemath.com/sample-variance-formula/
      val μ = vec.mean
      vec.map(i => (i - μ) * (i - μ)).sum / (vec.length - 1)
    end variance

    inline def stdDev: Double =
      // https://www.cuemath.com/data/standard-deviation/
      val mu = vec.mean
      val diffs_2 = vec.map(num => Math.pow(num - mu, 2))
      Math.sqrt(diffs_2.sum / (vec.length - 1))
    end stdDev

    inline def mean: Double = vec.sum / vec.length

    inline def sum: Double =
      var sum = 0.0
      var i = 0;
      while i < vec.length do
        sum = sum + vec(i)
        i = i + 1
      end while
      sum
    end sum

    inline def cumsum =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end cumsum

    inline def norm: Double = blas.cblas_dnrm2(vec.length, vec.at(0), 1)

    inline def dot(v1: Array[Double])(using inline boundsCheck: BoundsCheck): Double =
      dimCheck(vec, v1)
      blas.cblas_ddot(vec.length, vec.at(0), 1, v1.at(0), 1)
    end dot

    inline def -(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck) =
      dimCheck(vec, vec2)
      vec.clone.tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.cblas_daxpy(vec.length, -1.0, vec2.at(0), 1, vec.at(0), 1)
    end -=

    inline def +(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck) =
      dimCheck(vec, vec2)
      vec.clone.tap(_ += vec2)
    end +

    inline def +=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.cblas_daxpy(vec.length, 1.0, vec2.at(0), 1, vec.at(0), 1)
    end +=

    inline def +:+(d: Double) =
      vec.clone.tap(_ +:+= d)
    end +:+

    inline def +:+=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +:+=

    inline def add(d: Array[Double])(using inline boundsCheck: BoundsCheck) = vec + d
    inline def multInPlace(d: Double) = vec *= d

    inline def *=(d: Double) =
      blas.cblas_dscal(vec.length, d, vec.at(0), 1)
    end *=

    inline def *(d: Double) =
      vec.clone.tap(_ *= d)
    end *

    inline def <(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Double, Double) => Boolean,
        inline num: Double
    ): Array[Boolean] =
      val n = vec.length
      val idx = new Array[Boolean](n)
      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx

    def covariance(thatVector: Array[Double]): Double =
      val μThis = vec.mean
      val μThat = thatVector.mean
      var cv: Double = 0
      var i: Int = 0;
      while i < vec.length do
        cv += (vec(i) - μThis) * (thatVector(i) - μThat)
        i += 1
      end while
      cv / (vec.length - 1)
    end covariance

    // def max: Double = vec(blas.cblas_idamax(vec.length, vec.at(0), 1)) // No JS version
  end extension
  extension (vec: Array[Array[Double]])
    inline def horizontalSum: Array[Double] =
      val out = new Array[Double](vec.head.length)
      var i = 0
      while i < vec.head.length do
        var sum = 0.0
        var j = 0
        while j < vec.length do
          sum += vec(j)(i)
          // pprint.pprintln(s"j : $j i : $i vecij : ${vec(j)(i)}  out : ${out(i)} sum : $sum")
          j = j + 1
        end while
        out(i) = sum
        i = i + 1
      end while
      out
  end extension
end extensions
