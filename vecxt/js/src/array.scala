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

import scala.scalajs.js
import scala.scalajs.js.typedarray.Float64Array
import scala.util.chaining.*

import vecxt.BoundsCheck.BoundsCheck

import narr.*
import scala.reflect.ClassTag
import scala.scalajs.js.annotation.JSBracketAccess
import vecxt.JsNativeBooleanArrays.trues

object arrayUtil:
  extension [A](d: Array[A]) def printArr: String = d.mkString("[", ",", "]")
  end extension
end arrayUtil

object arrays:
  extension (d: Float64Array) def printArr: String = d.mkString("[", ",", "]")
  end extension

  extension (v: Float64Array)
    inline def nativeSort(): Unit = v.asInstanceOf[TypedArrayFacade].sort()
    inline def nativeReverse(): Unit = v.asInstanceOf[TypedArrayFacade].reverse()
    inline def nativeSlice(): Float64Array = v.asInstanceOf[TypedArrayFacade].slice()
  end extension

  @js.native
  trait TypedArrayFacade extends js.Object:

    def sort(): Unit = js.native
    def reverse(): Unit =
      js.native // mutable https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/reverse
    def slice(): Float64Array = js.native
  end TypedArrayFacade

  @js.native
  trait JsArrayFacade extends js.Object:
    def fill[A](a: A): Unit = js.native
  end JsArrayFacade

  extension [A](v: js.Array[A]) inline def fill(a: A): Unit = v.asInstanceOf[JsArrayFacade].fill(a)
  end extension
  extension (vec: js.Array[Boolean])
    // inline def trues: Int =
    //   var sum = 0
    //   for i <- 0 until vec.length do if vec(i) then sum = sum + 1
    //   end for
    //   sum
    // end trues

    inline def &&(thatIdx: js.Array[Boolean]): js.Array[Boolean] =
      val result = new js.Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) && thatIdx(i)
      end for
      result
    end &&

    inline def ||(thatIdx: js.Array[Boolean]): js.Array[Boolean] =
      val result = new js.Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) || thatIdx(i)
      end for
      result
    end ||
  end extension

  extension (vec: NArray[Double])
    inline def update(idx: Int, d: Double)(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      indexCheck(vec, idx)
      vec(idx) = d
    end update

  end extension

  extension (vec: NArray[Int])

    def apply(index: js.Array[Boolean]): NArray[Int] =
      val truely = index.trues
      val newVec = NArray.ofSize[Int](truely)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j += 1
      end for
      newVec
    end apply
  end extension

  extension (vec: NArray[Double])

    inline def apply(index: js.Array[Boolean])(using inline boundsCheck: BoundsCheck.BoundsCheck) =
      dimCheck(vec, index)
      val trues = index.trues
      val newVec = Float64Array(trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j = 1 + j
      end for
      newVec
    end apply

    def increments: NArray[Double] =
      val out = Float64Array(vec.length)
      out(0) = vec(0)
      var i = 1
      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out
    end increments

    inline def stdDev: Double =
      // https://www.cuemath.com/data/standard-deviation/
      val mu = vec.mean
      val diffs_2 = vec.map(num => (num - mu) * (num - mu))
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

    def variance: Double =
      // https://www.cuemath.com/sample-variance-formula/
      val μ = vec.mean
      vec.map(i => (i - μ) * (i - μ)).sum / (vec.length - 1)
    end variance

    inline def pearsonCorrelationCoefficient(thatVector: Float64Array)(using
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

    inline def spearmansRankCorrelation(thatVector: Float64Array)(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Double =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    // An alias - pearson is the most commonly requested type of correlation
    inline def corr(thatVector: Float64Array)(using inline boundsCheck: BoundsCheck.BoundsCheck): Double =
      pearsonCorrelationCoefficient(thatVector)

    def elementRanks: NArray[Double] =
      val indexed1 = vec.zipWithIndex
      val indexed = indexed1.toArray.sorted(Ordering.by(_._1))

      val ranks: Array[Double] = new Array(vec.length) // faster than zeros.
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
      Float64Array.of(ranks*)
    end elementRanks

    inline def cumsum =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end cumsum

    inline def dot(v1: NArray[Double])(using inline boundsCheck: BoundsCheck): Double =
      dimCheck(vec, v1)

      var product = 0.0
      var i = 0;
      while i < vec.length do
        product = product + vec(i) * v1(i)
        i = i + 1
      end while
      product
    end dot

    inline def norm: Double = blas.dnrm2(vec.length, vec, 1)

    inline def +(d: Double): NArray[Double] =
      vec.nativeSlice().tap(_ += d)

    inline def +=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    inline def -(d: Double): NArray[Double] =
      vec.nativeSlice().tap(_ -= d)
    end -

    inline def -=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    inline def -(vec2: NArray[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): NArray[Double] =
      dimCheck(vec, vec2)
      vec.nativeSlice().tap(_ -= vec2)
    end -

    inline def -=(vec2: NArray[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, -1.0, vec2, 1, vec, 1)
    end -=

    inline def +(vec2: NArray[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): NArray[Double] =
      dimCheck(vec, vec2)
      vec.nativeSlice().tap(_ += vec2)
    end +

    inline def +:+(d: Double) =
      vec.nativeSlice().tap(_ +:+= d)
    end +:+

    inline def +:+=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +:+=

    inline def +=(vec2: NArray[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, 1.0, vec2, 1, vec, 1)
    end +=

    inline def add(d: NArray[Double])(using inline boundsCheck: BoundsCheck): NArray[Double] = vec + d
    inline def multInPlace(d: Double): Unit = vec *= d

    inline def *=(d: Double): Unit =
      blas.dscal(vec.length, d, vec, 1)
    end *=

    inline def *(d: Double): NArray[Double] =
      vec.nativeSlice().tap(_ *= d)
    end *

    inline def /=(d: Double): NArray[Double] =
      vec.tap(v => blas.dscal(v.length, 1.0 / d, v, 1))
    end /=

    inline def /(d: Double): NArray[Double] =
      vec.nativeSlice().tap(_ /= d)
    end /

    def covariance(thatVector: NArray[Double]): Double =
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

    def maxElement: Double = vec.max
    // val t = js.Math.max( vec.toArray: _* )
  end extension

  extension (vec: Array[NArray[Double]])
    inline def horizontalSum: NArray[Double] =
      val out = new Float64Array(vec.head.length)
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
end arrays
