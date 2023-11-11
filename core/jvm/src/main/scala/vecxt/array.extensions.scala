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

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import vecxt.BoundsChecks.*
import vecxt.Limits.Limit
import vecxt.Retentions.Retention

import scala.util.chaining.*

package object vecxt:
  extension (vec: Array[Boolean])
    inline def countTrue: Int =
      var sum = 0
      for i <- 0 until vec.length do if vec(i) then sum = sum + 1
      sum
    end countTrue

    inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) && thatIdx(i)
      result
    end &&

    inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) || thatIdx(i)
      result
    end ||
  end extension

  extension (vec: Array[Double])

    inline def idxBoolean(index: Array[Boolean])(using inline boundsCheck: BoundsCheck) =
      dimCheck(vec, index)
      val trues = index.countTrue
      val newVec: Array[Double] = new Array[Double](trues)
      var j = 0
      for i <- 0 to trues do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j += 1
      end for
      newVec
    end idxBoolean

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

    inline def pearsonCorrelationCoefficient(thatVector: Array[Double])(using inline boundsCheck: BoundsCheck): Double =
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

    inline def spearmansRankCorrelation(thatVector: Array[Double])(using inline boundsCheck: BoundsCheck): Double =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    // An alias - pearson is the most commonly requested type of correlation
    inline def corr(thatVector: Array[Double])(using inline boundsCheck: BoundsCheck): Double =
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

    def variance: Double =
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

    inline def cumsum: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end cumsum

    inline def -(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, -1.0, vec2, 1, vec, 1)
    end -=

    inline def +(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ += vec2)
    end +

    inline def +=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, 1.0, vec2, 1, vec, 1)
    end +=

    inline def *=(d: Double): Array[Double] =
      vec.tap(v => blas.dscal(v.length, d, v, 1))
    end *=

    inline def *(d: Double): Array[Double] =
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

    /*

  Retention and limit are known constants

  f(X;retention, limit) = MIN(MAX(X - retention, 0), limit))

  Note: mutates the input array
     */
    inline def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i) - retention
            if tmp < 0.0 then vec(i) = 0.0
            else if tmp > limit then vec(i) = limit.toDouble
            else vec(i) = tmp
            end if
            i = i + 1
          end while

        case (None, Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i) - retention
            if tmp < 0.0 then vec(i) = 0.0
            else vec(i) = tmp
            i = i + 1
          end while

        case (Some(limit), None) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            if tmp > limit then vec(i) = limit.toDouble
            else vec(i) = tmp
            i = i + 1
          end while

        case (None, None) =>
          ()
    end reinsuranceFunction

    /*

  Retention and limit are known constants

  In excel f(x) = if(x < retention, 0, if(x > limit, limit, x)

     */
    inline def franchiseFunction(inline limitOpt: Option[Limit], inline retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (None, None) => ()

        case (Some(limit), Some(retention)) =>
          var i = 0;
          val maxLim = limit.toDouble + retention.toDouble
          while i < vec.length do
            val tmp = vec(i)
            if tmp < retention then vec(i) = 0.0
            else if tmp > maxLim then vec(i) = maxLim
            else vec(i) = tmp
            end if
            i = i + 1
          end while

        case (Some(limit), None) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            if tmp > limit.toDouble then vec(i) = limit.toDouble
            else vec(i) = tmp
            end if
            i = i + 1
          end while
        case (None, Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            if tmp > retention.toDouble then vec(i) = tmp
            else vec(i) = 0.0
            end if
            i = i + 1
          end while
    end franchiseFunction

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
end vecxt
