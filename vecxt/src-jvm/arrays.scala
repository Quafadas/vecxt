package vecxt

import scala.reflect.ClassTag
import scala.util.chaining.*

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.Matrix

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorMask
import jdk.incubator.vector.VectorOperators

object arrays:

  final val spi = IntVector.SPECIES_PREFERRED
  final val spd = DoubleVector.SPECIES_PREFERRED
  final val spb = ByteVector.SPECIES_PREFERRED

  final val spdl = spd.length()
  final val spbl = spb.length()
  final val spil = spi.length()

  /** Generates a vector of linearly spaced values between a and b (inclusive). The returned vector will have length
    * elements, defaulting to 100.
    */
  def linspace(a: Double, b: Double, length: Int = 100): Array[Double] =

    val result = new Array[Double](length)
    val increment = (b - a) / (length - 1)

    var i = 0
    val loopBound = spd.loopBound(length)

    // SIMD loop
    while i < loopBound do
      val indices = DoubleVector.fromArray(spd, Array.tabulate(spd.length())(j => (i + j).toDouble), 0)
      val values = indices.mul(increment).add(a)
      values.intoArray(result, i)
      i += spdl
    end while

    // Scalar tail
    while i < length do
      result(i) = a + increment * i
      i += 1
    end while

    result
  end linspace

  extension (vec: Array[Int])

    inline def =:=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.EQ, num)

    inline def !:=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.NE, num)

    inline def <(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.LT, num)

    inline def <=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.LE, num)

    inline def >(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.GT, num)

    inline def >=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.GE, num)

    inline def gte(num: Array[Int]): Array[Boolean] = >=(num)

    inline def lte(num: Array[Int]): Array[Boolean] = <=(num)

    inline def lt(num: Array[Int]): Array[Boolean] = <(num)

    inline def gt(num: Array[Int]): Array[Boolean] = >(num)

    inline def logicalIdx(
        inline op: VectorOperators.Comparison,
        vec2: Array[Int]
    ): Array[Boolean] =
      val idx = new Array[Boolean](vec.length)
      var i = 0

      while i < spi.loopBound(vec.length) do
        IntVector.fromArray(spi, vec, i).compare(op, IntVector.fromArray(spi, vec2, i)).intoArray(idx, i)
        i += spil
      end while

      inline op match
        case VectorOperators.EQ =>
          while i < vec.length do
            idx(i) = vec(i) == vec2(i)
            i += 1
          end while
        case VectorOperators.NE =>
          while i < vec.length do
            idx(i) = vec(i) != vec2(i)
            i += 1
          end while
        case VectorOperators.LT =>
          while i < vec.length do
            idx(i) = vec(i) < vec2(i)
            i += 1
          end while

        case VectorOperators.LE =>
          while i < vec.length do
            idx(i) = vec(i) <= vec2(i)
            i += 1
          end while

        case VectorOperators.GT =>
          while i < vec.length do
            idx(i) = vec(i) > vec2(i)
            i += 1
          end while

        case VectorOperators.GE =>
          while i < vec.length do
            idx(i) = vec(i) >= vec2(i)
            i += 1
          end while
        case _ => ???
      end match

      idx
    end logicalIdx

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

    inline def countsToIdx: Array[Int] =
      var total = vec.sumSIMD
      var i = 0
      val out = new Array[Int](total)
      var j = 0
      while i < vec.length do
        val count = vec(i)
        val idx = i + 1
        var k = 0
        while k < count do
          out(j) = idx
          j += 1
          k += 1
        end while
        i += 1
      end while
      out
    end countsToIdx

    inline def sumSIMD: Int =
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
    end sumSIMD

    inline def mean: Double =
      sumSIMD / vec.length.toDouble
    end mean

    inline def variance: Double = variance(VarianceMode.Population)

    inline def variance(mode: VarianceMode): Double =
      meanAndVariance(mode).variance

    inline def meanAndVariance: (mean: Double, variance: Double) =
      meanAndVariance(VarianceMode.Population)

    inline def meanAndVariance(mode: VarianceMode): (mean: Double, variance: Double) =
      meanAndVarianceTwoPass(mode)
    end meanAndVariance

    /** 231] Benchmark (len) Mode Cnt Score Error Units 231] VarianceBenchmark.var_simd_twopass 1000 thrpt 3 1087302.435
      * ± 16013.286 ops/s 231] VarianceBenchmark.var_simd_twopass 100000 thrpt 3 9578.869 ± 334.606 ops/s 231]
      * VarianceBenchmark.var_simd_welford 1000 thrpt 3 436244.559 ± 6158.585 ops/s 231]
      * VarianceBenchmark.var_simd_welford 100000 thrpt 3 4187.715 ± 203.266 ops/s
      */
    inline def meanAndVarianceTwoPass(mode: VarianceMode): (mean: Double, variance: Double) =
      val μ = vec.mean
      val μVec = DoubleVector.broadcast(spd, μ)

      var i = 0
      var acc = DoubleVector.zero(spd)
      val tmp = new Array[Double](spdl)

      while i < spd.loopBound(vec.length) do
        var lane = 0
        while lane < spdl do
          tmp(lane) = vec(i + lane).toDouble
          lane += 1
        end while

        val v = DoubleVector.fromArray(spd, tmp, 0)
        val diff = v.sub(μVec)
        acc = diff.fma(diff, acc)
        i += spdl
      end while

      var sumSqDiff = acc.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        val diff = vec(i).toDouble - μ
        sumSqDiff = Math.fma(diff, diff, sumSqDiff)
        i += 1
      end while

      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      (μ, sumSqDiff / denom)
    end meanAndVarianceTwoPass

    inline def std: Double = std(VarianceMode.Population)

    inline def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    inline def stdDev: Double = stdDev(VarianceMode.Population)

    inline def stdDev(mode: VarianceMode): Double = std(mode)

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

    inline def -=(scalar: Int): Unit =

      var i = 0

      while i < spi.loopBound(vec.length) do
        IntVector
          .fromArray(spi, vec, i)
          .sub(scalar)
          .intoArray(vec, i)
        i += spil
      end while

      while i < vec.length do
        vec(i) = vec(i) - scalar
        i += 1
      end while

    end -=

    inline def -(scalar: Int): Array[Int] =
      vec.clone().tap(_ -= scalar)
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

    inline def minSIMD =
      var i = 0
      var acc = IntVector.broadcast(spi, Int.MaxValue)

      while i < spi.loopBound(vec.length) do
        acc = acc.min(IntVector.fromArray(spi, vec, i))
        i += spil
      end while

      var temp = acc.reduceLanes(VectorOperators.MIN)

      while i < vec.length do
        temp = Math.min(temp, vec(i))
        i += 1
      end while
      temp
    end minSIMD

    inline def maxSIMD =
      var i = 0
      var acc = IntVector.broadcast(spi, Int.MinValue)

      while i < spi.loopBound(vec.length) do
        acc = acc.max(IntVector.fromArray(spi, vec, i))
        i += spil
      end while

      var temp = acc.reduceLanes(VectorOperators.MAX)

      while i < vec.length do
        temp = Math.max(temp, vec(i))
        i += 1
      end while
      temp
    end maxSIMD

  end extension

  extension (d: Double)
    inline def /(arr: Array[Double]) =
      val out = new Array[Double](arr.length)

      var i = 0
      while i < spd.loopBound(arr.length) do
        DoubleVector.broadcast(spd, d).div(DoubleVector.fromArray(spd, arr, i)).intoArray(out, i)
        i += spdl
      end while

      while i < arr.length do
        out(i) = d / arr(i)
        i = i + 1
      end while
      out
    end /

    inline def +(arr: Array[Double]): Array[Double] = arr.+(d)

    inline def -(arr: Array[Double]): Array[Double] =
      val out = new Array[Double](arr.length)
      var i = 0
      val bd = DoubleVector.broadcast(spd, d)
      while i < spd.loopBound(arr.length) do
        bd.sub(DoubleVector.fromArray(spd, arr, i)).intoArray(out, i)
        i += spdl
      end while

      while i < arr.length do
        out(i) = d - arr(i)
        i = i + 1
      end while
      out
    end -

    inline def *(arr: Array[Double]): Array[Double] = arr.*(d)

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
    private inline def unaryOp(inline op: VectorOperators.Unary): Unit =
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .lanewise(op)
          .intoArray(vec, i)
        i += spdl
      end while

      if i < vec.length then
        val mask = VectorMask.fromLong(spd, (1L << (vec.length - i)) - 1)
        DoubleVector
          .fromArray(spd, vec, i, mask)
          .lanewise(op)
          .intoArray(vec, i, mask)
      end if
    end unaryOp

    inline def unary_- : Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.NEG))

    inline def -! : Unit =
      unaryOp(VectorOperators.NEG)

    inline def abs: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.ABS))

    inline def `abs!`: Unit =
      unaryOp(VectorOperators.ABS)

    inline def acos: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.ACOS))

    inline def `acos!`: Unit =
      unaryOp(VectorOperators.ACOS)

    inline def asin: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.ASIN))

    inline def `asin!`: Unit =
      unaryOp(VectorOperators.ASIN)

    inline def atan: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.ATAN))

    inline def `atan!`: Unit =
      unaryOp(VectorOperators.ATAN)

    inline def cbrt: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.CBRT))

    inline def `cbrt!`: Unit =
      unaryOp(VectorOperators.CBRT)

    inline def cos: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.COS))

    inline def `cos!`: Unit =
      unaryOp(VectorOperators.COS)

    inline def cosh: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.COSH))

    inline def `cosh!`: Unit =
      unaryOp(VectorOperators.COSH)

    inline def exp: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.EXP))

    inline def `exp!`: Unit =
      unaryOp(VectorOperators.EXP)

    inline def expm1: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.EXPM1))

    inline def `expm1!`: Unit =
      unaryOp(VectorOperators.EXPM1)

    inline def log: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.LOG))

    inline def `log!`: Unit =
      unaryOp(VectorOperators.LOG)

    inline def log10: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.LOG10))

    inline def `log10!`: Unit =
      unaryOp(VectorOperators.LOG10)

    inline def log1p: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.LOG1P))

    inline def `log1p!`: Unit =
      unaryOp(VectorOperators.LOG1P)

    inline def sqrt: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.SQRT))

    inline def `sqrt!`: Unit =
      unaryOp(VectorOperators.SQRT)

    inline def sin: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.SIN))

    inline def `sin!`: Unit =
      unaryOp(VectorOperators.SIN)

    inline def sinh: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.SINH))

    inline def `sinh!`: Unit =
      unaryOp(VectorOperators.SINH)

    inline def `tan!`: Unit =
      unaryOp(VectorOperators.TAN)

    inline def tan: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.TAN))

    inline def `tanh!`: Unit =
      unaryOp(VectorOperators.TANH)

    inline def tanh: Array[Double] =
      vec.clone().tap(_.unaryOp(VectorOperators.TANH))

    inline def `**!`(power: Double): Unit =
      var i = 0
      val bp = DoubleVector.broadcast(spd, power)
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .lanewise(VectorOperators.POW, bp)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = Math.pow(vec(i), power)
        i += 1
      end while
    end `**!`

    inline def **(power: Double): Array[Double] =
      vec.clone().tap(_.`**!`(power))

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
        val x = vec(i)
        val y = thatVector(i)
        // Use fma to optimize multiply-add operations for better performance
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = Math.fma(x, y, sum_xy) // x * y + sum_xy
        sum_x2 = Math.fma(x, x, sum_x2) // x * x + sum_x2
        sum_y2 = Math.fma(y, y, sum_y2) // y * y + sum_y2
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
      indexed.sortInPlace()(using Ordering.by(_._1))

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

    inline def variance: Double = variance(VarianceMode.Population)

    def variance(mode: VarianceMode): Double =
      meanAndVariance(mode).variance
    end variance

    inline def std: Double = std(VarianceMode.Population)

    inline def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    inline def stdDev: Double = stdDev(VarianceMode.Population)

    inline def stdDev(mode: VarianceMode): Double = std(mode)

    inline def meanAndVariance: (mean: Double, variance: Double) =
      meanAndVariance(VarianceMode.Population)

    inline def meanAndVariance(mode: VarianceMode): (mean: Double, variance: Double) =
      meanAndVarianceTwoPass(mode)
    end meanAndVariance

    /** True SIMD-optimized Welford's algorithm for computing mean and variance.
      *
      * Each SIMD lane maintains independent Welford accumulators (n, mean, M2). Lanes process strided elements: lane 0
      * gets [0,4,8,...], lane 1 gets [1,5,9,...], etc. At the end, all lanes are merged using the parallel Welford
      * merge formula:
      *
      * δ = meanB - meanA n = nA + nB mean = meanA + δ * nB / n M2 = M2A + M2B + δ² * nA * nB / n
      *
      * This algorimth is crushed by the simple two pass SIMD version.
      *
      * 231] Benchmark (len) Mode Cnt Score Error Units 231] VarianceBenchmark.var_simd_twopass 1000 thrpt 3 1087302.435
      * ± 16013.286 ops/s 231] VarianceBenchmark.var_simd_twopass 100000 thrpt 3 9578.869 ± 334.606 ops/s 231]
      * VarianceBenchmark.var_simd_welford 1000 thrpt 3 436244.559 ± 6158.585 ops/s 231]
      * VarianceBenchmark.var_simd_welford 100000 thrpt 3 4187.715 ± 203.266 ops/s
      */
    private inline def meanAndVarianceWelfordSIMD(mode: VarianceMode): (mean: Double, variance: Double) =
      if vec.length == 0 then (0.0, 0.0)
      else
        // Per-lane accumulators
        var laneMeans = DoubleVector.zero(spd)
        var delta = DoubleVector.zero(spd)
        var delta2 = DoubleVector.zero(spd)
        var laneM2 = DoubleVector.zero(spd)

        var i = 0
        var j: Double = 1
        // ALl lanes will have processed J elements at the end of this loop
        while i < spd.loopBound(vec.length) do
          j = j + 1
          val values = DoubleVector.fromArray(spd, vec, i)
          delta = values.sub(laneMeans) // Use current mean
          laneMeans = laneMeans.add(delta.div(DoubleVector.broadcast(spd, j)))
          delta2 = values.sub(laneMeans) // Use updated mean
          laneM2 = laneM2.add(delta.mul(delta2))
          i += spdl
        end while

        // val laneSumA = laneSum.toArray()
        val laneMean = laneMeans.toArray()
        val laneM2A = laneM2.toArray()
        // Merge all lanes
        var globalN = j
        var globalMean = laneMean(0)
        var globalM2 = laneM2A(0)

        var lane = 1
        while lane < spdl do
          val delta = laneMean(lane) - globalMean
          val newN = globalN + j
          globalMean = globalMean + delta * j / newN
          globalM2 = globalM2 + laneM2A(lane) + delta * delta * globalN * j / newN
          globalN = newN

          lane += 1
        end while

        // Process tail elements
        while i < vec.length do
          val n = globalN + 1
          val delta = vec(i) - globalMean
          globalMean += delta / n
          val delta2 = vec(i) - globalMean
          globalM2 += delta * delta2
          globalN = n
          i += 1
        end while

        val denom = mode match
          case VarianceMode.Population => vec.length.toDouble
          case VarianceMode.Sample     => (vec.length - 1).toDouble

        (globalMean, globalM2 / denom)
      end if
    end meanAndVarianceWelfordSIMD

    /** Two-pass variance calculation (legacy, for comparison). First pass computes mean, second pass computes variance.
      */
    inline def meanAndVarianceTwoPass(mode: VarianceMode): (mean: Double, variance: Double) =
      val μ = vec.mean
      val l = spd.length()
      var tmp = DoubleVector.zero(spd)
      val μVec = DoubleVector.broadcast(spd, μ)

      var i = 0
      while i < spd.loopBound(vec.length) do
        val v = DoubleVector.fromArray(spd, vec, i)
        val diff = v.sub(μVec)
        tmp = diff.fma(diff, tmp)
        i += spdl
      end while

      var sumSqDiff = tmp.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        val diff = vec(i) - μ
        sumSqDiff = Math.fma(diff, diff, sumSqDiff)
        i += 1
      end while

      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      (μ, sumSqDiff / denom)
    end meanAndVarianceTwoPass

    inline def mean: Double = vec.sumSIMD / vec.length

    inline def sum: Double = sumSIMD

    inline def sumSIMD: Double =
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
    end sumSIMD

    inline def product: Double = productSIMD

    inline def productSIMD: Double =
      var i: Int = 0
      var acc = DoubleVector.broadcast(spd, 1.0)

      while i < spd.loopBound(vec.length) do
        acc = acc.mul(DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while
      var temp = acc.reduceLanes(VectorOperators.MUL)
      // var temp = 0.0
      while i < vec.length do
        temp *= vec(i)
        i += 1
      end while
      temp
    end productSIMD

    /** Given an array `nums` of n integers where n > 1, returns an array `output` such that `output[i]` is equal to the
      * product of all the elements of `nums` except `nums[i]`.
      *
      * This method does not use division and runs in O(n) time complexity.
      *
      * @param nums
      *   An array of integers.
      * @return
      *   An array where each element is the product of all the elements of `nums` except the element at the same index.
      */
    inline def productExceptSelf: Array[Double] =
      val n = vec.length
      val leftProducts = new Array[Double](n)
      val rightProducts = new Array[Double](n)

      leftProducts(0) = 1.0
      rightProducts(n - 1) = 1.0

      var i = 1
      var j = n - 2
      while i < n do
        leftProducts(i) = leftProducts(i - 1) * vec(i - 1)
        rightProducts(j) = rightProducts(j + 1) * vec(j + 1)
        i += 1
        j -= 1
      end while

      i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, leftProducts, i)
          .mul(DoubleVector.fromArray(spd, rightProducts, i))
          .intoArray(leftProducts, i)
        i += spdl
      end while

      while i < vec.length do
        leftProducts(i) = leftProducts(i) * rightProducts(i)
        i = i + 1
      end while

      leftProducts
    end productExceptSelf

    private inline def reduceOp(inline op: VectorOperators.Binary, inline initial: Double): Double =
      var i = 0
      var vecAcc = DoubleVector.broadcast(spd, initial)

      while i < spd.loopBound(vec.length) do
        vecAcc = vecAcc.lanewise(op, DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while

      var result = vecAcc.reduceLanes(op.asInstanceOf[VectorOperators.Associative])

      while i < vec.length do
        result = inline op match
          case VectorOperators.MAX => Math.max(result, vec(i))
          case VectorOperators.MIN => Math.min(result, vec(i))
          case _                   => ???
        i += 1
      end while

      result
    end reduceOp

    inline def max: Double = maxSIMD

    inline def min: Double = minSIMD

    inline def maxSIMD: Double =
      reduceOp(VectorOperators.MAX, Double.MinValue)

    inline def minSIMD: Double =
      reduceOp(VectorOperators.MIN, Double.MaxValue)

    private inline def `clampOp!`(inline op: VectorOperators.Comparison, inline initial: Double): Unit =
      var i = 0
      var vecAcc = DoubleVector.broadcast(spd, initial)

      while i < spd.loopBound(vec.length) do
        val values = DoubleVector.fromArray(spd, vec, i)
        val mask = values.compare(op, initial)
        vecAcc.intoArray(vec, i, mask)
        values.intoArray(vec, i, mask.not())
        i += spdl
      end while

      while i < vec.length do
        vec(i) = inline op match
          case VectorOperators.LT => Math.max(initial, vec(i))
          case VectorOperators.GT => Math.min(initial, vec(i))
          case _                  => ???
        i += 1
      end while

    end `clampOp!`

    /** Clamps the values in the array to a maximum value.
      *
      * @param floor
      *   The maximum value to clamp to.
      * @return
      *   A new array with values clamped to the specified maximum.
      */
    inline def clampMax(ceil: Double): Array[Double] = vec.clone.tap(_.`clampOp!`(VectorOperators.GT, ceil))
    inline def maxClamp(ceil: Double): Array[Double] = vec.clone.tap(_.`clampOp!`(VectorOperators.GT, ceil))
    inline def `maxClamp!`(ceil: Double): Unit =
      vec.`clampOp!`(VectorOperators.GT, ceil)

    /** Clamps the values in the array to a minimum value.
      *
      * @param ceil
      *   The minimum value to clamp to.
      * @return
      *   A new array with values clamped to the specified minimum.
      */
    inline def clampMin(floor: Double): Array[Double] = vec.clone.tap(_.`clampOp!`(VectorOperators.LT, floor))
    inline def minClamp(floor: Double): Array[Double] = vec.clone.tap(_.`clampOp!`(VectorOperators.LT, floor))
    inline def `minClamp!`(floor: Double): Unit =
      vec.`clampOp!`(VectorOperators.LT, floor)

    /** Clamps the values in the array to a specified range.
      * @param ceil
      *   The maximum value to clamp to.
      * @param floor
      *   The minimum value to clamp to.
      * @return
      *   A new array with values clamped to the specified range.
      */
    inline def `clamp!`(floor: Double, ceil: Double): Unit =
      var i = 0
      var vecCeil = DoubleVector.broadcast(spd, ceil)
      var vecFloor = DoubleVector.broadcast(spd, floor)

      while i < spd.loopBound(vec.length) do
        val values = DoubleVector.fromArray(spd, vec, i)
        val maskGt = values.compare(VectorOperators.GT, vecCeil)
        val maskLt = values.compare(VectorOperators.LT, vecFloor)
        vecCeil.intoArray(vec, i, maskGt)
        vecFloor.intoArray(vec, i, maskLt)
        values.intoArray(vec, i, maskGt.or(maskLt).not())
        i += spdl
      end while

      while i < vec.length do
        vec(i) = if vec(i) > ceil then ceil else if vec(i) < floor then floor else vec(i)
        i += 1
      end while

    end `clamp!`

    /** Clamps the values in the array to a specified range.
      * @param ceil
      *   The maximum value to clamp to.
      * @param floor
      *   The minimum value to clamp to.
      * @return
      *   A new array with values clamped to the specified range.
      */
    inline def clamp(floor: Double, ceil: Double): Array[Double] =
      vec.clone.tap(_.`clamp!`(floor, ceil))

    /** The formula for the logarithm of the sum of exponentials is:
      *
      * logSumExp(x) = log(sum(exp(x_i))) for i = 1 to n
      *
      * This is computed in a numerically stable way by subtracting the maximum value in the array before taking the
      * exponentials:
      *
      * logSumExp(x) = max(x) + log(sum(exp(x_i - max(x)))) for i = 1 to n
      */
    inline def logSumExp: Double =
      val maxVal = vec.max
      var sumExpVec = DoubleVector.zero(spd)
      var i = 0

      while i < spd.loopBound(vec.length) do
        val vecSegment = DoubleVector.fromArray(spd, vec, i)
        val expSegment = vecSegment.sub(maxVal).lanewise(VectorOperators.EXP)
        sumExpVec = sumExpVec.add(expSegment)
        i += spdl
      end while

      var sumExp = sumExpVec.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        sumExp += Math.exp(vec(i) - maxVal)
        i += 1
      end while

      maxVal + Math.log(sumExp)
    end logSumExp

    inline def `cumsum!`: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    inline def cumsum: Array[Double] =
      val out = vec.clone()
      out.`cumsum!`
      out
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

    inline def +(d: Double): Array[Double] =
      val out = new Array[Double](vec.length)
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .add(inc)
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) + d
        i = i + 1
      end while
      out
    end +

    inline def +=(d: Double): Unit =
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .add(inc)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    inline def -(d: Double): Array[Double] =
      val out = new Array[Double](vec.length)
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(inc)
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) - d
        i = i + 1
      end while
      out
    end -

    inline def `fma!`(multiply: Double, add: Double): Unit =
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .fma(multiply, add)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = Math.fma(vec(i), multiply, add)
        i = i + 1
      end while
    end `fma!`

    inline def fma(multiply: Double, add: Double): Array[Double] =
      vec.clone().tap(_ `fma!` (multiply, add))

    inline def -=(d: Double): Unit =
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(inc)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length - 1 do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

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

    inline def *(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, d)
      val out = new Array[Double](vec.length)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .mul(DoubleVector.fromArray(spd, d, i))
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *

    inline def *=(d: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, d)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .mul(DoubleVector.fromArray(spd, d, i))
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = vec(i) * d(i)
        i = i + 1
      end while
    end *=

    inline def /(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, d)
      val out = new Array[Double](vec.length)
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .div(DoubleVector.fromArray(spd, d, i))
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) / d(i)
        i = i + 1
      end while
      out
    end /

    inline def /=(d: Double): Array[Double] =
      vec.tap(v => blas.dscal(v.length, 1.0 / d, v, 1))
    end /=

    inline def /(d: Double): Array[Double] =
      vec.clone.tap(_ /= d)
    end /

    inline def *=(d: Double): Unit =
      var i = 0
      while i < spd.loopBound(vec.length) do
        DoubleVector
          .fromArray(spd, vec, i)
          .mul(DoubleVector.broadcast(spd, d))
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) *= d
        i += 1
      end while
    end *=

    inline def *(d: Double): Array[Double] =
      vec.clone.tap(_ *= d)
    end *

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
      val n = vec.length
      var i = 0
      var acc = DoubleVector.zero(spd)

      // SIMD loop
      while i < spd.loopBound(n) do
        val v1 = DoubleVector.fromArray(spd, vec, i).sub(μThis)
        val v2 = DoubleVector.fromArray(spd, thatVector, i).sub(μThat)
        acc = v1.fma(v2, acc)
        i += spdl
      end while

      // Remainder
      var cv = acc.reduceLanes(VectorOperators.ADD)
      while i < n do
        cv = Math.fma(vec(i) - μThis, thatVector(i) - μThat, cv)
        i += 1
      end while

      cv / (n - 1)
    end covariance

    /** Returns the index of the maximum element in the array using SIMD operations for performance.
      *
      * This method processes the array in blocks to maximize instruction-level parallelism (ILP) and minimize
      * synchronization overhead.
      *
      * https://en.algorithmica.org/hpc/algorithms/argmin/
      *
      * For small arrays, perhaps 2x slower. For larger arrays (e.g. 1000 elements, at least 2x faster)
      *
      * @return
      *   The index of the maximum element, or -1 if the array is empty.
      */
    def argmax: Int =
      val n = vec.length
      if n == 0 then return -1
      end if
      if n == 1 then return 0
      end if

      // Algorithmica.org approach: block-based with infrequent updates
      val blockSize = spd.length() * 4 // Process many elements per block for optimal ILP
      var globalMax = Double.MinValue
      var blockWithMax = 0

      var i = 0
      val loopBound = n - (n % blockSize)

      // Broadcast current max for SIMD comparison
      var maxVec = DoubleVector.broadcast(spd, globalMax)

      // Main SIMD loop processing 32 elements per iteration
      while i < loopBound do
        // Load 4 SIMD vectors (32 elements total)
        val v1 = DoubleVector.fromArray(spd, vec, i)
        val v2 = DoubleVector.fromArray(spd, vec, i + spdl)
        val v3 = DoubleVector.fromArray(spd, vec, i + 2 * spdl)
        val v4 = DoubleVector.fromArray(spd, vec, i + 3 * spdl)

        // Find block maximum using tree reduction
        val max12 = v1.max(v2)
        val max34 = v3.max(v4)
        val blockMax = max12.max(max34)

        // Check if any element in this block is greater than global max
        val mask = blockMax.compare(VectorOperators.GT, maxVec)

        if mask.anyTrue() then // Check if any element is greater - rarely executed
          // Update global maximum within this block
          var j = i
          while j < i + blockSize do
            if vec(j) > globalMax then globalMax = vec(j)
            end if
            j += 1
          end while
          blockWithMax = i
          maxVec = DoubleVector.broadcast(spd, globalMax)
        end if

        i += blockSize
      end while

      // Handle remaining elements
      while i < n do
        if vec(i) > globalMax then
          globalMax = vec(i)
          blockWithMax = (i / blockSize) * blockSize // Start of block containing this element
        end if
        i += 1
      end while

      // Find exact index within the block containing the maximum
      var exactIdx = blockWithMax
      val searchEnd = Math.min(blockWithMax + blockSize, n)
      var j = blockWithMax
      while j < searchEnd do
        if vec(j) == globalMax then
          exactIdx = j
          return exactIdx // Return first occurrence
        end if
        j += 1
      end while

      exactIdx
    end argmax

    /** Returns the index of the minimum element in the array using SIMD operations for performance.
      *
      * This method processes the array in blocks to maximize instruction-level parallelism (ILP) and minimize
      * synchronization overhead.
      *
      * For small arrays, perhaps 2x slower. For larger arrays (e.g. 1000 elements, at least 2x faster)
      *
      * @return
      *   The index of the minimum element, or -1 if the array is empty.
      */
    def argmin: Int =
      val n = vec.length
      if n == 0 then return -1
      end if
      if n == 1 then return 0
      end if

      val blockSize = spd.length() * 4
      var globalMin = Double.MaxValue
      var blockWithMin = 0

      var i = 0
      val loopBound = n - (n % blockSize)
      var minVec = DoubleVector.broadcast(spd, globalMin)

      while i < loopBound do
        // Load 4 SIMD vectors (32 elements total)
        val v1 = DoubleVector.fromArray(spd, vec, i)
        val v2 = DoubleVector.fromArray(spd, vec, i + spdl)
        val v3 = DoubleVector.fromArray(spd, vec, i + 2 * spdl)
        val v4 = DoubleVector.fromArray(spd, vec, i + 3 * spdl)

        // Find block maximum using tree reduction
        val max12 = v1.max(v2)
        val max34 = v3.max(v4)
        val blockMax = max12.max(max34)

        // Check if any element in this block is greater than global max
        val mask = blockMax.compare(VectorOperators.LT, minVec)

        if mask.anyTrue() then // Check if any element is greater - rarely executed
          // Update global maximum within this block
          var j = i
          while j < i + blockSize do
            if vec(j) < globalMin then globalMin = vec(j)
            end if
            j += 1
          end while
          blockWithMin = i
          minVec = DoubleVector.broadcast(spd, globalMin)
        end if

        i += blockSize
      end while

      while i < n do
        if vec(i) < globalMin then
          globalMin = vec(i)
          blockWithMin = (i / blockSize) * blockSize
        end if

        i += 1
      end while

      var exactIdx = blockWithMin
      val searchEnd = Math.min(blockWithMin + blockSize, n)
      var j = blockWithMin
      while j < searchEnd do
        if vec(j) == globalMin then
          exactIdx = j
          return exactIdx
        end if
        j += 1
      end while

      exactIdx
    end argmin

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
          // Use fma to optimize accumulation: vec(j)(i) * 1.0 + sum
          sum = Math.fma(vec(j)(i), 1.0, sum)
          // pprint.pprintln(s"j : $j i : $i vecij : ${vec(j)(i)}  out : ${out(i)} sum : $sum")
          j = j + 1
        end while
        out(i) = sum
        i = i + 1
      end while
      out
  end extension
end arrays
