package vecxt

import scala.reflect.ClassTag

import vecxt.annotations.AllocFree
import vecxt.annotations.HotPath
import vecxt.annotations.Thin

import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.FloatVector
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.VectorSpecies
import scala.annotation.targetName

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

    // `logicalIdx` keeps `inline` — the comparison operator has to reach C2 as a constant, and the `inline op match`
    // that picks the scalar tail is a compile error if it cannot reduce. These six do not need it: each writes its
    // operator literally at its own definition site, so the reduction still happens inside the emitted body. What
    // `inline` bought was a copy of the SIMD loop and the selected tail in every caller.
    //
    // `@HotPath`, not `@Thin`: the expanded `logicalIdx` brings a loop with it, and the budget that matters is
    // `FreqInlineSize`. No `@AllocFree` — every one of them returns a fresh `Array[Boolean]`.
    @HotPath
    def =:=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.EQ, num)

    @HotPath
    def !:=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.NE, num)

    @HotPath
    def <(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.LT, num)

    @HotPath
    def <=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.LE, num)

    @HotPath
    def >(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.GT, num)

    @HotPath
    def >=(num: Array[Int]): Array[Boolean] =
      logicalIdx(VectorOperators.GE, num)

    // The named aliases are `@Thin` rather than `@HotPath`: now that the operator forms above are emitted, each of
    // these is one real call and carries no loop of its own.
    @Thin
    def gte(num: Array[Int]): Array[Boolean] = >=(num)

    @Thin
    def lte(num: Array[Int]): Array[Boolean] = <=(num)

    @Thin
    def lt(num: Array[Int]): Array[Boolean] = <(num)

    @Thin
    def gt(num: Array[Int]): Array[Boolean] = >(num)

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

    // The scalar-argument mirror of the block above, same reasoning throughout.
    @HotPath
    def =:=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.EQ, num)

    @HotPath
    def !:=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.NE, num)

    @HotPath
    def <(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.LT, num)

    @HotPath
    def <=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.LE, num)

    @HotPath
    def >(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.GT, num)

    @HotPath
    def >=(num: Int): Array[Boolean] =
      logicalIdx(VectorOperators.GE, num)

    @Thin
    def gte(num: Int): Array[Boolean] = >=(num)

    @Thin
    def lte(num: Int): Array[Boolean] = <=(num)

    @Thin
    def lt(num: Int): Array[Boolean] = <(num)

    @Thin
    def gt(num: Int): Array[Boolean] = >(num)

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

    @HotPath
    def increments: Array[Int] =
      val out = new Array[Int](vec.length)
      val limit = spi.loopBound(vec.length - 2)
      // val inc = spil - 1
      // val maskInit = spi.maskAll(true).toArray()
      // maskInit(maskInit.length - 1) = false
      // val mask = VectorMask.fromArray(spi, maskInit, 0)

      var i = 1
      val bound = spi.loopBound(vec.length - 2)
      while i < bound do
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

    @HotPath
    def countsToIdx: Array[Int] =
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

    @HotPath
    @AllocFree
    def sumSIMD: Int =
      var i: Int = 0
      var acc = IntVector.zero(spi)

      val bound = spi.loopBound(vec.length)
      while i < bound do
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

    // The statistics surface is forwarders all the way down to `meanAndVarianceTwoPass`, which was already a plain
    // `def` and does the work. `inline` on the forwarders duplicated the chain into every caller to save calls that
    // C2 makes free anyway, each one being well inside `MaxInlineSize`.
    @Thin
    def mean: Double =
      sumSIMD / vec.length.toDouble
    end mean

    @Thin
    def variance: Double = variance(VarianceMode.Population)

    @Thin
    def variance(mode: VarianceMode): Double =
      meanAndVariance(mode).variance

    @Thin
    def meanAndVariance: (mean: Double, variance: Double) =
      meanAndVariance(VarianceMode.Population)

    @Thin
    def meanAndVariance(mode: VarianceMode): (mean: Double, variance: Double) =
      meanAndVarianceTwoPass(mode)
    end meanAndVariance

    /** 231] Benchmark (len) Mode Cnt Score Error Units 231] VarianceBenchmark.var_simd_twopass 1000 thrpt 3 1087302.435
      * ± 16013.286 ops/s 231] VarianceBenchmark.var_simd_twopass 100000 thrpt 3 9578.869 ± 334.606 ops/s 231]
      * VarianceBenchmark.var_simd_welford 1000 thrpt 3 436244.559 ± 6158.585 ops/s 231]
      * VarianceBenchmark.var_simd_welford 100000 thrpt 3 4187.715 ± 203.266 ops/s
      */
    def meanAndVarianceTwoPass(mode: VarianceMode): (mean: Double, variance: Double) =
      val μ = vec.mean
      val μVec = DoubleVector.broadcast(spd, μ)

      var i = 0
      var acc = DoubleVector.zero(spd)
      val tmp = new Array[Double](spdl)
      val bound = spd.loopBound(vec.length)
      while i < bound do
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

    @Thin
    def std: Double = std(VarianceMode.Population)

    @Thin
    def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    @Thin
    def stdDev: Double = stdDev(VarianceMode.Population)

    @Thin
    def stdDev(mode: VarianceMode): Double = std(mode)

    @HotPath
    @AllocFree
    def dot(vec2: Array[Int]): Int =
      dimCheck(vec, vec2)
      var i = 0
      var acc = IntVector.zero(spi)
      val bound = spi.loopBound(vec.length)
      while i < bound do
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

    // `dimCheck` + `clone` + delegate to the annotated in-place kernel. The estimate is ~25 bytecodes, so `@Thin`
    // should hold, but this shape sits closer to `MaxInlineSize` (35) than anything else in the file — if C3 rejects
    // one of these three, the number it reports is the answer for the whole `dimCheck`-and-delegate population, not a
    // reason to put `inline` back.
    @Thin
    def -(vec2: Array[Int]): Array[Int] =
      dimCheck(vec, vec2)
      val out = vec.clone
      out -= vec2
      out
    end -

    @HotPath
    @AllocFree
    def -=(scalar: Int): Unit =
      var i = 0
      val bound = spi.loopBound(vec.length)
      while i < bound do
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

    @targetName("divideByDoubleScalar")
    def /(scalar: Double): Array[Double] =
      val result = new Array[Double](vec.length)
      val scalarDoubleVec = DoubleVector.broadcast(spd, scalar)
      val tmp = new Array[Double](spdl)

      var i = 0

      val bound = spd.loopBound(vec.length)
      while i < bound do
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

    def /(scalar: Float): Array[Float] =
      val result = new Array[Float](vec.length)
      val scalarFloatVec = FloatVector.broadcast(spf, scalar)
      val tmp = new Array[Float](spfl)

      var i = 0
      val bound = spf.loopBound(vec.length)
      while i < bound do
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

    def *(scalar: Float): Array[Float] =
      val result = new Array[Float](vec.length)
      val scalarFloatVec = FloatVector.broadcast(spf, scalar)
      val tmp = new Array[Float](spfl)

      var i = 0

      val bound = spf.loopBound(vec.length)
      while i < bound do
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

    @Thin
    def -(scalar: Int): Array[Int] =
      val out = vec.clone()
      out -= scalar
      out
    end -

    @HotPath
    @AllocFree
    def -=(vec2: Array[Int]): Unit =
      dimCheck(vec, vec2)
      var i = 0
      val bound = spi.loopBound(vec.length)
      while i < bound do
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

    @Thin
    def +(vec2: Array[Int]): Array[Int] =
      dimCheck(vec, vec2)
      val out = vec.clone
      out += vec2
      out
    end +

    @HotPath
    @AllocFree
    def +=(vec2: Array[Int]): Unit =
      dimCheck(vec, vec2)
      var i = 0
      val bound = spi.loopBound(vec.length)
      while i < bound do
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

    @HotPath
    @AllocFree
    def minSIMD =
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

    @HotPath
    @AllocFree
    def maxSIMD =
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
