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

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import scala.util.chaining.*

import jdk.incubator.vector.VectorMask
import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorShape
import jdk.incubator.vector.VectorOperators
import scala.compiletime.constValue

import vecxt.BoundsCheck.BoundsCheck
import scala.compiletime.ops.double
import scala.annotation.static

object arrays:

  extension (vec: Array[Boolean])
    inline def countTrue: Int =
      val species = ByteVector.SPECIES_PREFERRED
      val l = species.length()
      var sum = 0
      var i = 0
      while i < species.loopBound(vec.length) do
        sum = sum + ByteVector.fromBooleanArray(species, vec, i).reduceLanes(VectorOperators.ADD)
        i += l
      end while

      while i < vec.length do
        if vec(i) then sum += 1
        end if
        i += 1
      end while
      sum
    end countTrue

    inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val species = ByteVector.SPECIES_PREFERRED
      val l = species.length()
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < species.loopBound(vec.length) do
        ByteVector
          .fromBooleanArray(species, vec, i)
          .and(ByteVector.fromBooleanArray(species, thatIdx, i))
          .intoBooleanArray(result, i)
        i += l
      end while

      while i < vec.length do
        result(i) = vec(i) && thatIdx(i)
        i += 1
      end while
      result
    end &&

    inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =
      val species = ByteVector.SPECIES_PREFERRED
      val l = species.length()
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < species.loopBound(vec.length) do
        ByteVector
          .fromBooleanArray(species, vec, i)
          .or(ByteVector.fromBooleanArray(species, thatIdx, i))
          .intoBooleanArray(result, i)
        i += l
      end while

      while i < vec.length do
        result(i) = vec(i) || thatIdx(i)
        i += 1
      end while
      result
    end ||
  end extension

  extension (vec: Array[Double])

    inline def apply(index: Array[Boolean])(using inline boundsCheck: BoundsCheck) =
      dimCheck(vec, index)
      val trues = index.countTrue
      val newVec: Array[Double] = new Array[Double](trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j = 1 + j
      end for
      newVec
    end apply

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
      val sp = DoubleVector.SPECIES_PREFERRED
      val l = sp.length()

      var i = 1
      while i < sp.loopBound(vec.length - 2) do
        val v1 = DoubleVector.fromArray(sp, vec, i)
        val v2 = DoubleVector.fromArray(sp, vec, i + 1)
        v2.sub(v1).intoArray(out, i)
        i += l
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

    def variance: Double =
      // https://www.cuemath.com/sample-variance-formula/
      val μ = vec.mean
      // vec.map(i => (i - μ) * (i - μ)).sum / (vec.length - 1)
      val sp = DoubleVector.SPECIES_PREFERRED
      val l = sp.length()
      var tmp = DoubleVector.zero(sp)

      var i = 0
      while i < sp.loopBound(vec.length) do
        val v = DoubleVector.fromArray(sp, vec, i)
        val diff = v.sub(μ)
        tmp = tmp.add(diff.mul(diff))
        i += l
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
      val sp = DoubleVector.SPECIES_PREFERRED
      var acc = DoubleVector.zero(sp)

      val l = sp.length()

      while i < sp.loopBound(vec.length) do
        acc = acc.add(DoubleVector.fromArray(sp, vec, i))
        i += l
      end while
      var temp = acc.reduceLanes(VectorOperators.ADD)
      // var temp = 0.0
      while i < vec.length do
        temp += vec(i)
        i += 1
      end while
      temp
    end sum

    // https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-39-parallel-prefix-sum-scan-cuda#:~:text=A%20simple%20and%20common%20parallel%20algorithm#:~:text=A%20simple%20and%20common%20parallel%20algorithm
    inline def cumsum: Unit =
      val spd: VectorSpecies[java.lang.Double] = DoubleVector.SPECIES_PREFERRED
      val spi: VectorSpecies[java.lang.Integer] =
        VectorSpecies.of(java.lang.Integer.TYPE, VectorShape.forBitSize(spd.vectorBitSize() / 2))

      def doubleLength = spd.length()
      // println(doubleLength)
      def intLength = spi.length()
      // println(intLength)
      // println(vec.length)

      var dBound = Math.log(vec.length) / Math.log(2)
      val simdFor = Math.pow(2, dBound.toInt).toInt
      // println(s"simdFor ${simdFor}")
      // println(s"dboud ${dBound - 1}")

      var d: Int = 0
      while d < (dBound - 1) do
        var k: Int = 0
        val dPow2 = Math.pow(2, d).toInt
        val dPow2_1 = Math.pow(2, d + 1).toInt
        // println("---------------------------------")
        // println(s"--d loop $d")
        // println(dPow2)
        // println(dPow2_1)

        while k < spi.loopBound((vec.length - 1)) do
          val idxs = IntVector.broadcast(spi, k).addIndex(dPow2_1)

          val x_idx = idxs.add(dPow2_1 - 1)
          val part1Idx = idxs.add(dPow2 - 1)
          val part2Idx = idxs.add(dPow2_1 - 1)

          val mask = part2Idx.lt(simdFor).cast(spd)
          // println(s"indexes")
          // println(s"k: $k")
          // println(idxs.toArray().mkString(","))
          // println(x_idx.toArray().mkString(","))
          // println(part1Idx.toArray().mkString(","))
          // println(part2Idx.toArray().mkString(","))

          val part1 = DoubleVector.fromArray(spd, vec, 0, part1Idx.toArray(), 0, mask)
          val part2 = DoubleVector.fromArray(spd, vec, 0, part2Idx.toArray(), 0, mask)

          // println(s"new vecs")
          // println(part1.toArray().mkString(","))
          // println(part2.toArray().mkString(","))

          // println(s"combined")
          // println(part1.add(part2).toArray().mkString(","))
          part1.add(part2).intoArray(vec, 0, x_idx.toArray(), 0, mask)
          // println(s"new vec")
          // println(vec.mkString(","))
          // println(s" $k complete ")
          // println("=====")
          k += (intLength * dPow2_1)
        end while
        d += 1
      end while
      // println(Math.pow(2, dBound.toInt))

      // println("UP SWEEP COMPLETE")
      // println(vec.mkString("[", ",", "]"))
      // println("-----")

      d = d - 1

      val notfirst = Array.fill[Boolean](spd.length())(true)
      notfirst(0) = false

      val asMask = VectorMask.fromArray(spi, notfirst, 0)

      while d > 0 do

        val dPow2 = Math.pow(2, d).toInt
        val dPow2_2 = Math.pow(2, d - 2).toInt
        val dPow2_1 = Math.pow(2, d - 1).toInt
        // println("---dssss")
        // println(s"d : $d")
        // println(s"dPow2_1 : $dPow2_1")
        var k = 0
        while k < spi.loopBound(vec.length - 1) do

          val idxs = IntVector.broadcast(spi, k).addIndex(dPow2)
          val idxsInsert = IntVector.broadcast(spi, k).addIndex(dPow2).add(dPow2_1).sub(1)
          // println("idxs")
          // println(idxs.sub(1).toArray().mkString(","))
          // println(idxsInsert.toArray().mkString(","))

          val mask = idxs.compare(
            VectorOperators.LT,
            simdFor
          )

          val finalM = mask.and(asMask).cast(spd)

          // println("mask")
          // println(finalM.toArray().mkString(","))

          val xtract = DoubleVector.fromArray(spd, vec, 0, idxs.sub(1).toArray(), 0, finalM)
          // println("xtract")
          // println(xtract.toArray().mkString(","))
          val current = DoubleVector.fromArray(spd, vec, 0, idxsInsert.toArray(), 0, finalM)
          // println("current")
          // println(current.toArray().mkString(","))

          current.add(xtract).intoArray(vec, 0, idxsInsert.toArray(), 0, finalM)
          // println("ITRT END")
          // println(vec.mkString(","))
          // println("----- END")

          k += intLength * dPow2_1
        end while
        d -= 1
      end while

      var i = Math.pow(2, dBound.toInt).toInt
      println(s"tail at $i")
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
      println(vec.toArray.mkString("[", ",", "]"))
    end cumsum

    def cumsum2: Array[Double] =
      val vec2 = vec.clone()
      var i = 1
      while i < vec2.length do
        vec2(i) = vec2(i - 1) + vec2(i)
        i = i + 1
      end while
      vec2
    end cumsum2

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
      val species = DoubleVector.SPECIES_PREFERRED
      val l = species.length()
      val idx = new Array[Boolean](vec.length)
      var i = 0

      while i < species.loopBound(vec.length) do
        DoubleVector.fromArray(species, vec, i).compare(op, num).intoArray(idx, i)
        i += l
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
