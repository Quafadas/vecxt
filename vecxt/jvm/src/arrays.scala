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

import scala.util.chaining.*

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.Matrix

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorMask
import scala.reflect.ClassTag

object arrays:

  final val spi = IntVector.SPECIES_PREFERRED
  final val spd = DoubleVector.SPECIES_PREFERRED
  final val spb = ByteVector.SPECIES_PREFERRED

  final val spdl = spd.length()
  final val spbl = spb.length()
  final val spil = spi.length()

  extension (vec: Array[Boolean])
    // Inefficient as it doesn't break if a false is found.
    inline def all: Boolean =
      var acc = ByteVector.broadcast(spb, 1.toByte)
      var i = 0
      while i < spb.loopBound(vec.length) do
        acc = acc.and(ByteVector.fromBooleanArray(spb, vec, i))
        i += spbl
      end while

      var out = acc.reduceLanes(VectorOperators.AND) > 0

      if out then
        while i < vec.length do
          if !vec(i) then out = false
          end if
          i += 1
        end while

      end if
      out
    end all

    inline def any: Boolean =
      var acc = ByteVector.zero(spb)
      var i = 0
      while i < spb.loopBound(vec.length) do
        acc = acc.or(ByteVector.fromBooleanArray(spb, vec, i))
        i += spbl
      end while

      var out = acc.reduceLanes(VectorOperators.OR) > 0

      if !out then
        while i < vec.length do
          if vec(i) then out = true
          end if
          i += 1
        end while

      end if
      out
    end any

    inline def trues: Int =
      var i = 0
      var sum = 0
      while i < vec.length do
        if vec(i) then sum += 1
        end if
        i += 1
      end while
      sum
    end trues

    inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < spb.loopBound(vec.length) do
        ByteVector
          .fromBooleanArray(spb, vec, i)
          .and(ByteVector.fromBooleanArray(spb, thatIdx, i))
          .intoBooleanArray(result, i)
        i += spbl
      end while

      while i < vec.length do
        result(i) = vec(i) && thatIdx(i)
        i += 1
      end while
      result
    end &&

    inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =

      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < spb.loopBound(vec.length) do
        ByteVector
          .fromBooleanArray(spb, vec, i)
          .or(ByteVector.fromBooleanArray(spb, thatIdx, i))
          .intoBooleanArray(result, i)
        i += spbl
      end while

      while i < vec.length do
        result(i) = vec(i) || thatIdx(i)
        i += 1
      end while
      result
    end ||
  end extension

  extension (vec: Array[Int])

    inline def =:=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.EQ, num)

    inline def !:=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.NE, num)

    inline def <(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.LT, num)

    inline def <=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.LE, num)

    inline def >(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.GT, num)

    inline def >=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.GE, num)

    inline def gte(num: Int): Array[Boolean] = >=(num)

    inline def lte(num: Int): Array[Boolean] = <=(num)

    inline def lt(num: Int): Array[Boolean] = <(num)

    inline def gt(num: Int): Array[Boolean] = >(num)

    inline def logicalIdx(
        inline op: VectorOperators.Comparison,
        num: Int
    ): Array[Boolean] =
      val idx = new Array[Boolean](vec.length)
      var i = 0

      while i < spi.loopBound(vec.length) do
        IntVector.fromArray(spi, vec, i).compare(op, num).intoArray(idx, i)
        i += spil
      end while

      inline op match
        case VectorOperators.EQ =>
          while i < vec.length do
            idx(i) = vec(i) == num
            i += 1
          end while
        case VectorOperators.NE =>
          while i < vec.length do
            idx(i) = vec(i) != num
            i += 1
          end while
        case VectorOperators.LT =>
          while i < vec.length do
            idx(i) = vec(i) < num
            i += 1
          end while

        case VectorOperators.LE =>
          while i < vec.length do
            idx(i) = vec(i) <= num
            i += 1
          end while

        case VectorOperators.GT =>
          while i < vec.length do
            idx(i) = vec(i) > num
            i += 1
          end while

        case VectorOperators.GE =>
          while i < vec.length do
            idx(i) = vec(i) >= num
            i += 1
          end while
        case _ => ???
      end match

      idx
    end logicalIdx

    inline def increments: Array[Int] =
      val out = new Array[Int](vec.length)
      val limit = spi.loopBound(vec.length - 2)
      // val inc = spil - 1
      // val maskInit = spi.maskAll(true).toArray()
      // maskInit(maskInit.length - 1) = false
      // val mask = VectorMask.fromArray(spi, maskInit, 0)

      var i = 1
      while i < spi.loopBound(vec.length - 2) do
        IntVector.fromArray(spi, vec, i).sub(IntVector.fromArray(spi, vec, i - 1)).intoArray(out, i)
        i += spil
      end while

      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out(0) = vec(0)
      out

    end increments

    inline def sum: Int =
      var i: Int = 0
      var acc = IntVector.zero(spi)

      while i < spi.loopBound(vec.length) do
        acc = acc.add(IntVector.fromArray(spi, vec, i))
        i += spil
      end while
      var temp = acc.reduceLanes(VectorOperators.ADD)
      // var temp = 0.0
      while i < vec.length do
        temp += vec(i)
        i += 1
      end while
      temp
    end sum

    inline def dot(vec2: Array[Int])(using inline boundsCheck: BoundsCheck): Int =
      dimCheck(vec, vec2)
      val newVec = Array.ofDim[Int](vec.length)
      var i = 0
      var acc = IntVector.zero(spi)

      while i < spi.loopBound(vec.length) do
        acc = IntVector
          .fromArray(spi, vec, i)
          .mul(IntVector.fromArray(spi, vec2, i))
          .add(acc)

        i += spil
      end while

      var temp = acc.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        temp += vec(i) * vec2(i)
        i += 1
      end while
      temp
    end dot

    inline def -(vec2: Array[Int])(using inline boundsCheck: BoundsCheck): Array[Int] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Int])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      var i = 0

      while i < spi.loopBound(vec.length) do
        IntVector
          .fromArray(spi, vec, i)
          .sub(IntVector.fromArray(spi, vec2, i))
          .intoArray(vec, i)
        i += spil
      end while

      while i < vec.length do
        vec(i) = vec(i) - vec2(i)
        i += 1
      end while
    end -=

    inline def +(vec2: Array[Int])(using inline boundsCheck: BoundsCheck): Array[Int] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ += vec2)
    end +

    inline def +=(vec2: Array[Int])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      var i = 0

      while i < spi.loopBound(vec.length) do
        IntVector
          .fromArray(spi, vec, i)
          .add(IntVector.fromArray(spi, vec2, i))
          .intoArray(vec, i)
        i += spil
      end while

      while i < vec.length do
        vec(i) = vec(i) + vec2(i)
        i += 1
      end while
    end +=

  end extension

  extension [@specialized(Double, Int) A](vec: Array[A])(using ClassTag[A])
    inline def apply(index: Array[Boolean])(using inline boundsCheck: BoundsCheck) =
      dimCheck(vec, index)
      val trues = index.trues
      val newVec: Array[A] = new Array[A](trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j = 1 + j
      end for
      newVec
    end apply
  end extension

  extension (vec: Array[Double])

    /** Apparently, left packing is hard problem in SIMD land.
      * https://stackoverflow.com/questions/79025873/selecting-values-from-java-simd-doublevector
      */

    // inline def apply(index: Array[Boolean])(using inline boundsCheck: BoundsCheck): Array[Double] =
    //   dimCheck(vec, index)
    //   val newVec: Array[Double] = new Array[Double](index.length)
    //   val out = new Array[Double](vec.length)
    //   val sp = Matrix.doubleSpecies
    //   val l = sp.length()

    //   var i = 0
    //   var j = 0
    //   while i < sp.loopBound(vec.length) do
    //     println(s"i: $i  || j: $j")
    //     val mask = VectorMask.fromArray[java.lang.Double](sp, index, i)

    //     val vals = DoubleVector
    //       .fromArray(sp, vec, i)

    //     // val selected = vals.selectFrom(vals, mask)

    //     println(s"mask: ${mask.toArray().print}")
    //     println(s"vals: ${vals.toArray().print}")
    //     vals.intoArray(newVec, j, mask)
    //     println(newVec.print)

    //     i += l
    //     j = j + mask.trueCount()

    //   end while

    //   while i < vec.length do
    //     if index(i) then
    //       newVec(j) = vec(i)
    //       j += 1
    //     end if
    //     i += 1
    //   end while

    //   newVec

    // end apply

    inline def increments: Array[Double] =
      val out = new Array[Double](vec.length)

      var i = 1
      while i < spd.loopBound(vec.length - 2) do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(DoubleVector.fromArray(spd, vec, i - 1))
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out(0) = vec(0)
      out
    end increments

    inline def pearsonCorrelationCoefficient(thatVector: Array[Double])(using
        inline boundsCheck: BoundsCheck
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

    inline def outer(other: Array[Double])(using ClassTag[Double]): Matrix[Double] =
      val n = vec.length
      val m = other.length
      val out = new Array[Double](n * m)

      var j = 0
      while j < m do
        var i = 0
        val tmp = DoubleVector.broadcast(spd, other(j))
        while i < spd.loopBound(n) do
          DoubleVector.fromArray(spd, vec, i).mul(tmp).intoArray(out, j * n + i)
          i = i + spdl
        end while

        while i < n do
          out(j * n + i) = vec(i) * other(j)
          i = i + 1
        end while
        j = j + 1
      end while
      Matrix(out, (n, m))(using BoundsCheck.DoBoundsCheck.no)
    end outer

    def variance: Double =
      // https://www.cuemath.com/sample-variance-formula/
      val μ = vec.mean
      // vec.map(i => (i - μ) * (i - μ)).sum / (vec.length - 1)

      val l = spd.length()
      var tmp = DoubleVector.zero(spd)

      var i = 0
      while i < spd.loopBound(vec.length) do
        val v = DoubleVector.fromArray(spd, vec, i)
        val diff = v.sub(μ)
        tmp = tmp.add(diff.mul(diff))
        i += spdl
      end while

      var sumSqDiff = tmp.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        sumSqDiff = sumSqDiff + (vec(i) - μ) * (vec(i) - μ)
        i += 1
      end while

      sumSqDiff / (vec.length - 1)

    end variance

    inline def stdDev: Double =
      // https://www.cuemath.com/data/standard-deviation/
      val mu = vec.mean
      val diffs_2 = vec.map(num => Math.pow(num - mu, 2))
      Math.sqrt(diffs_2.sum / (vec.length - 1))
    end stdDev

    inline def mean: Double = vec.sum / vec.length

    inline def sum: Double =
      var i: Int = 0
      var acc = DoubleVector.zero(spd)

      while i < spd.loopBound(vec.length) do
        acc = acc.add(DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while
      var temp = acc.reduceLanes(VectorOperators.ADD)
      // var temp = 0.0
      while i < vec.length do
        temp += vec(i)
        i += 1
      end while
      temp
    end sum

    inline def cumsum: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end cumsum

    inline def dot(v1: Array[Double])(using inline boundsCheck: BoundsCheck): Double =
      dimCheck(vec, v1)
      blas.ddot(vec.length, vec, 1, v1, 1)
    end dot

    inline def norm: Double = blas.dnrm2(vec.length, vec, 1)

    inline def -(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, -1.0, vec2, 1, vec, 1)
    end -=

    inline def add(d: Array[Double])(using inline boundsCheck: BoundsCheck) = vec + d

    inline def +(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ += vec2)
    end +

    inline def +=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, 1.0, vec2, 1, vec, 1)
    end +=

    inline def +:+(d: Double): Array[Double] =
      vec.clone.tap(_ +:+= d)
    end +:+

    inline def +:+=(d: Double): Unit =
      var i: Int = 0
      while i < vec.length do
        vec(i) += d
        i += 1
      end while
    end +:+=

    inline def multInPlace(d: Double) = vec *= d

    inline def *=(d: Double): Array[Double] =
      vec.tap(v => blas.dscal(v.length, d, v, 1))
    end *=

    inline def *(d: Double): Array[Double] =
      vec.clone.tap(_ *= d)
    end *

    inline def /=(d: Double): Array[Double] =
      vec.tap(v => blas.dscal(v.length, 1.0 / d, v, 1))
    end /=

    inline def /(d: Double): Array[Double] =
      vec.clone.tap(_ /= d)
    end /

    inline def =:=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.EQ, num)

    inline def !:=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.NE, num)

    inline def <(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.LT, num)

    inline def <=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.LE, num)

    inline def >(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.GT, num)

    inline def >=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.GE, num)

    inline def logicalIdx(
        inline op: VectorOperators.Comparison,
        num: Double
    ): Array[Boolean] =
      val idx = new Array[Boolean](vec.length)
      var i = 0

      while i < spd.loopBound(vec.length) do
        DoubleVector.fromArray(spd, vec, i).compare(op, num).intoArray(idx, i)
        i += spdl
      end while

      inline op match
        case VectorOperators.EQ =>
          while i < vec.length do
            idx(i) = vec(i) == num
            i += 1
          end while
        case VectorOperators.NE =>
          while i < vec.length do
            idx(i) = vec(i) != num
            i += 1
          end while
        case VectorOperators.LT =>
          while i < vec.length do
            idx(i) = vec(i) < num
            i += 1
          end while

        case VectorOperators.LE =>
          while i < vec.length do
            idx(i) = vec(i) <= num
            i += 1
          end while

        case VectorOperators.GT =>
          while i < vec.length do
            idx(i) = vec(i) > num
            i += 1
          end while

        case VectorOperators.GE =>
          while i < vec.length do
            idx(i) = vec(i) >= num
            i += 1
          end while
        case _ => ???
      end match

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

    // def max: Double =
    //   vec(blas.idamax(vec.length, vec, 1)) // No JS version
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
end arrays
