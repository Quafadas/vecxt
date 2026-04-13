package vecxt

import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import vecxt.BoundsCheck.BoundsCheck

import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.FloatVector
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.VectorSpecies

object intarrays:

  private final val spi: VectorSpecies[Integer] = IntVector.SPECIES_PREFERRED
  private final val spd: VectorSpecies[java.lang.Double] = DoubleVector.SPECIES_PREFERRED
  private final val spb = ByteVector.SPECIES_PREFERRED
  private final val spf = FloatVector.SPECIES_PREFERRED

  private final val spdl = spd.length()
  private final val spbl = spb.length()
  private final val spfl = spf.length()
  private final val spil: Int = spi.length()

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

    inline def /(scalar: Double): Array[Double] =
      val result = new Array[Double](vec.length)
      val scalarDoubleVec = DoubleVector.broadcast(spd, scalar)
      val tmp = new Array[Double](spdl)

      var i = 0

      while i < spd.loopBound(vec.length) do
        var lane = 0
        while lane < spdl do
          tmp(lane) = vec(i + lane).toDouble
          lane += 1
        end while

        DoubleVector
          .fromArray(spd, tmp, 0)
          .div(scalarDoubleVec)
          .intoArray(result, i)

        i += spdl
      end while

      while i < vec.length do
        result(i) = vec(i) / scalar
        i += 1
      end while

      result

    end /

    inline def /(scalar: Float): Array[Float] =
      val result = new Array[Float](vec.length)
      val scalarFloatVec = FloatVector.broadcast(spf, scalar)
      val tmp = new Array[Float](spfl)

      var i = 0

      while i < spf.loopBound(vec.length) do
        var lane = 0
        while lane < spfl do
          tmp(lane) = vec(i + lane).toFloat
          lane += 1
        end while

        FloatVector
          .fromArray(spf, tmp, 0)
          .div(scalarFloatVec)
          .intoArray(result, i)
        i += spfl
      end while

      while i < vec.length do
        result(i) = vec(i) / scalar
        i += 1
      end while

      result
    end /

    inline def *(scalar: Float): Array[Float] =
      val result = new Array[Float](vec.length)
      val scalarFloatVec = FloatVector.broadcast(spf, scalar)
      val tmp = new Array[Float](spfl)

      var i = 0

      while i < spf.loopBound(vec.length) do
        var lane = 0
        while lane < spfl do
          tmp(lane) = vec(i + lane).toFloat
          lane += 1
        end while

        FloatVector
          .fromArray(spf, tmp, 0)
          .mul(scalarFloatVec)
          .intoArray(result, i)
        i += spfl
      end while

      while i < vec.length do
        result(i) = vec(i) * scalar
        i += 1
      end while

      result
    end *

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

end intarrays
