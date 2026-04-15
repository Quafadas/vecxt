package vecxt

import scala.reflect.ClassTag
import scala.util.chaining.*

import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.Matrix

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.FloatVector
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorMask
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.VectorSpecies

/** Cross compilation shim
  */
object JsNativeFloatArrays

object floatarrays:

  private final val spi: VectorSpecies[Integer] = IntVector.SPECIES_PREFERRED
  private final val spf: VectorSpecies[java.lang.Float] = FloatVector.SPECIES_PREFERRED
  private final val spb: VectorSpecies[java.lang.Byte] = ByteVector.SPECIES_PREFERRED
  private final val spfl: Int = spf.length()
  private final val spbl: Int = spb.length()
  private final val spil: Int = spi.length()

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

  extension (f: Float)
    inline def /(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      val bf = FloatVector.broadcast(spf, f)
      var i = 0
      while i < spf.loopBound(arr.length) do
        bf.div(FloatVector.fromArray(spf, arr, i)).intoArray(out, i)
        i += spfl
      end while

      while i < arr.length do
        out(i) = f / arr(i)
        i = i + 1
      end while
      out
    end /

    inline def +(arr: Array[Float]): Array[Float] = arr.+(f)

    inline def -(arr: Array[Float]): Array[Float] =
      val out = new Array[Float](arr.length)
      var i = 0
      val bf = FloatVector.broadcast(spf, f)
      while i < spf.loopBound(arr.length) do
        bf.sub(FloatVector.fromArray(spf, arr, i)).intoArray(out, i)
        i += spfl
      end while

      while i < arr.length do
        out(i) = f - arr(i)
        i = i + 1
      end while
      out
    end -

    inline def *(arr: Array[Float]): Array[Float] = arr.*(f)

  end extension

  extension (vec: Array[Float])

    private inline def unaryFloatOp(inline op: VectorOperators.Unary): Unit =
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .lanewise(op)
          .intoArray(vec, i)
        i += spfl
      end while

      if i < vec.length then
        val mask = VectorMask.fromLong(spf, (1L << (vec.length - i)) - 1)
        FloatVector
          .fromArray(spf, vec, i, mask)
          .lanewise(op)
          .intoArray(vec, i, mask)
      end if
    end unaryFloatOp

    inline def unary_- : Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.NEG))

    inline def `-!`: Unit =
      unaryFloatOp(VectorOperators.NEG)

    inline def abs: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.ABS))

    inline def `abs!`: Unit =
      unaryFloatOp(VectorOperators.ABS)

    inline def acos: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.ACOS))

    inline def `acos!`: Unit =
      unaryFloatOp(VectorOperators.ACOS)

    inline def asin: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.ASIN))

    inline def `asin!`: Unit =
      unaryFloatOp(VectorOperators.ASIN)

    inline def atan: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.ATAN))

    inline def `atan!`: Unit =
      unaryFloatOp(VectorOperators.ATAN)

    inline def cbrt: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.CBRT))

    inline def `cbrt!`: Unit =
      unaryFloatOp(VectorOperators.CBRT)

    inline def cos: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.COS))

    inline def `cos!`: Unit =
      unaryFloatOp(VectorOperators.COS)

    inline def cosh: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.COSH))

    inline def `cosh!`: Unit =
      unaryFloatOp(VectorOperators.COSH)

    inline def exp: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.EXP))

    inline def `exp!`: Unit =
      unaryFloatOp(VectorOperators.EXP)

    inline def expm1: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.EXPM1))

    inline def `expm1!`: Unit =
      unaryFloatOp(VectorOperators.EXPM1)

    inline def log: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.LOG))

    inline def `log!`: Unit =
      unaryFloatOp(VectorOperators.LOG)

    inline def log10: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.LOG10))

    inline def `log10!`: Unit =
      unaryFloatOp(VectorOperators.LOG10)

    inline def log1p: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.LOG1P))

    inline def `log1p!`: Unit =
      unaryFloatOp(VectorOperators.LOG1P)

    inline def sqrt: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.SQRT))

    inline def `sqrt!`: Unit =
      unaryFloatOp(VectorOperators.SQRT)

    inline def sin: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.SIN))

    inline def `sin!`: Unit =
      unaryFloatOp(VectorOperators.SIN)

    inline def sinh: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.SINH))

    inline def `sinh!`: Unit =
      unaryFloatOp(VectorOperators.SINH)

    inline def tan: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.TAN))

    inline def `tan!`: Unit =
      unaryFloatOp(VectorOperators.TAN)

    inline def tanh: Array[Float] =
      vec.clone().tap(_.unaryFloatOp(VectorOperators.TANH))

    inline def `tanh!`: Unit =
      unaryFloatOp(VectorOperators.TANH)

    inline def `**!`(power: Float): Unit =
      var i = 0
      val bp = FloatVector.broadcast(spf, power)
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .lanewise(VectorOperators.POW, bp)
          .intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        vec(i) = Math.pow(vec(i).toDouble, power.toDouble).toFloat
        i += 1
      end while
    end `**!`

    inline def **(power: Float): Array[Float] =
      vec.clone().tap(_.`**!`(power))

    inline def `fma!`(multiply: Float, add: Float): Unit =
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .fma(multiply, add)
          .intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        vec(i) = vec(i) * multiply + add
        i = i + 1
      end while
    end `fma!`

    inline def fma(multiply: Float, add: Float): Array[Float] =
      vec.clone().tap(_ `fma!` (multiply, add))

    inline def clampMin(floor: Float): Array[Float] = vec.clone.tap(_.`clampFloatOp!`(VectorOperators.LT, floor))
    inline def minClamp(floor: Float): Array[Float] = vec.clone.tap(_.`clampFloatOp!`(VectorOperators.LT, floor))
    inline def `minClamp!`(floor: Float): Unit =
      vec.`clampFloatOp!`(VectorOperators.LT, floor)

    inline def clampMax(ceil: Float): Array[Float] = vec.clone.tap(_.`clampFloatOp!`(VectorOperators.GT, ceil))
    inline def maxClamp(ceil: Float): Array[Float] = vec.clone.tap(_.`clampFloatOp!`(VectorOperators.GT, ceil))
    inline def `maxClamp!`(ceil: Float): Unit =
      vec.`clampFloatOp!`(VectorOperators.GT, ceil)

    private inline def `clampFloatOp!`(inline op: VectorOperators.Comparison, inline initial: Float): Unit =
      var i = 0
      var vecAcc = FloatVector.broadcast(spf, initial)

      while i < spf.loopBound(vec.length) do
        val values = FloatVector.fromArray(spf, vec, i)
        val mask = values.compare(op, initial)
        vecAcc.intoArray(vec, i, mask)
        values.intoArray(vec, i, mask.not())
        i += spfl
      end while

      while i < vec.length do
        vec(i) = inline op match
          case VectorOperators.LT => Math.max(initial, vec(i))
          case VectorOperators.GT => Math.min(initial, vec(i))
          case _                  => ???
        i += 1
      end while

    end `clampFloatOp!`

    inline def `clamp!`(floor: Float, ceil: Float): Unit =
      var i = 0
      val vecCeil = FloatVector.broadcast(spf, ceil)
      val vecFloor = FloatVector.broadcast(spf, floor)

      while i < spf.loopBound(vec.length) do
        val values = FloatVector.fromArray(spf, vec, i)
        val maskGt = values.compare(VectorOperators.GT, vecCeil)
        val maskLt = values.compare(VectorOperators.LT, vecFloor)
        vecCeil.intoArray(vec, i, maskGt)
        vecFloor.intoArray(vec, i, maskLt)
        values.intoArray(vec, i, maskGt.or(maskLt).not())
        i += spfl
      end while

      while i < vec.length do
        vec(i) = if vec(i) > ceil then ceil else if vec(i) < floor then floor else vec(i)
        i += 1
      end while

    end `clamp!`

    inline def clamp(floor: Float, ceil: Float): Array[Float] =
      vec.clone.tap(_.`clamp!`(floor, ceil))

    inline def argmax: Int =
      val n = vec.length
      if n == 0 then -1
      else
        var maxIdx = 0
        var maxVal = vec(0)
        var i = 1
        while i < n do
          if vec(i) > maxVal then
            maxVal = vec(i)
            maxIdx = i
          end if
          i += 1
        end while
        maxIdx
      end if
    end argmax

    inline def argmin: Int =
      val n = vec.length
      if n == 0 then -1
      else
        var minIdx = 0
        var minVal = vec(0)
        var i = 1
        while i < n do
          if vec(i) < minVal then
            minVal = vec(i)
            minIdx = i
          end if
          i += 1
        end while
        minIdx
      end if
    end argmin

    private inline def reduceFloatOp(inline op: VectorOperators.Binary, inline initial: Float): Float =
      var i = 0
      var vecAcc = FloatVector.broadcast(spf, initial)

      while i < spf.loopBound(vec.length) do
        vecAcc = vecAcc.lanewise(op, FloatVector.fromArray(spf, vec, i))
        i += spfl
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
    end reduceFloatOp

    inline def max: Float = maxSIMD

    inline def min: Float = minSIMD

    inline def maxSIMD: Float =
      reduceFloatOp(VectorOperators.MAX, Float.MinValue)

    inline def minSIMD: Float =
      reduceFloatOp(VectorOperators.MIN, Float.MaxValue)

    inline def sumSIMD: Float =
      var i: Int = 0
      var acc = FloatVector.zero(spf)

      while i < spf.loopBound(vec.length) do
        acc = acc.add(FloatVector.fromArray(spf, vec, i))
        i += spfl
      end while
      var temp = acc.reduceLanes(VectorOperators.ADD)
      while i < vec.length do
        temp += vec(i)
        i += 1
      end while
      temp
    end sumSIMD

    inline def sum: Float = sumSIMD

    inline def productSIMD: Float =
      var i: Int = 0
      var acc = FloatVector.broadcast(spf, 1.0f)

      while i < spf.loopBound(vec.length) do
        acc = acc.mul(FloatVector.fromArray(spf, vec, i))
        i += spfl
      end while
      var temp = acc.reduceLanes(VectorOperators.MUL)
      while i < vec.length do
        temp *= vec(i)
        i += 1
      end while
      temp
    end productSIMD

    inline def product: Float = productSIMD

    inline def productExceptSelf: Array[Float] =
      val n = vec.length
      val leftProducts = new Array[Float](n)
      val rightProducts = new Array[Float](n)

      leftProducts(0) = 1.0f
      rightProducts(n - 1) = 1.0f

      var i = 1
      var j = n - 2
      while i < n do
        leftProducts(i) = leftProducts(i - 1) * vec(i - 1)
        rightProducts(j) = rightProducts(j + 1) * vec(j + 1)
        i += 1
        j -= 1
      end while

      i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, leftProducts, i)
          .mul(FloatVector.fromArray(spf, rightProducts, i))
          .intoArray(leftProducts, i)
        i += spfl
      end while

      while i < vec.length do
        leftProducts(i) = leftProducts(i) * rightProducts(i)
        i = i + 1
      end while

      leftProducts
    end productExceptSelf

    inline def mean: Float = vec.sumSIMD / vec.length

    inline def meanAndVariance: (mean: Float, variance: Float) =
      meanAndVariance(VarianceMode.Population)

    inline def meanAndVariance(mode: VarianceMode): (mean: Float, variance: Float) =
      meanAndVarianceTwoPass(mode)
    end meanAndVariance

    inline def meanAndVarianceTwoPass(mode: VarianceMode): (mean: Float, variance: Float) =
      val μ = vec.mean.toDouble
      val μVec = FloatVector.broadcast(spf, μ.toFloat)

      var i = 0
      var acc = FloatVector.zero(spf)

      while i < spf.loopBound(vec.length) do
        val v = FloatVector.fromArray(spf, vec, i)
        val diff = v.sub(μVec)
        acc = diff.fma(diff, acc)
        i += spfl
      end while

      var sumSqDiff = acc.reduceLanes(VectorOperators.ADD).toDouble

      while i < vec.length do
        val diff = vec(i).toDouble - μ
        sumSqDiff += diff * diff
        i += 1
      end while

      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      (μ.toFloat, (sumSqDiff / denom).toFloat)
    end meanAndVarianceTwoPass

    inline def variance: Float = variance(VarianceMode.Population)

    inline def variance(mode: VarianceMode): Float =
      meanAndVariance(mode).variance
    end variance

    inline def std: Float = std(VarianceMode.Population)

    inline def std(mode: VarianceMode): Float =
      Math.sqrt(vec.variance(mode).toDouble).toFloat

    inline def stdDev: Float = stdDev(VarianceMode.Population)

    inline def stdDev(mode: VarianceMode): Float = std(mode)

    inline def dot(v1: Array[Float])(using inline boundsCheck: BoundsCheck): Float =
      dimCheck(vec, v1)
      blas.sdot(vec.length, vec, 1, v1, 1)
    end dot

    inline def norm: Float = blas.snrm2(vec.length, vec, 1)

    inline def increments: Array[Float] =
      val out = new Array[Float](vec.length)

      var i = 1
      while i < spf.loopBound(vec.length - 2) do
        FloatVector
          .fromArray(spf, vec, i)
          .sub(FloatVector.fromArray(spf, vec, i - 1))
          .intoArray(out, i)
        i += spfl
      end while

      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out(0) = vec(0)
      out
    end increments

    inline def `cumsum!`: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    inline def cumsum: Array[Float] =
      val out = vec.clone()
      out.`cumsum!`
      out
    end cumsum

    inline def logSumExp: Float =
      val maxVal = vec.max
      var sumExpVec = FloatVector.zero(spf)
      var i = 0

      while i < spf.loopBound(vec.length) do
        val vecSegment = FloatVector.fromArray(spf, vec, i)
        val expSegment = vecSegment.sub(maxVal).lanewise(VectorOperators.EXP)
        sumExpVec = sumExpVec.add(expSegment)
        i += spfl
      end while

      var sumExp = sumExpVec.reduceLanes(VectorOperators.ADD).toDouble

      while i < vec.length do
        sumExp += Math.exp((vec(i) - maxVal).toDouble)
        i += 1
      end while

      (maxVal + Math.log(sumExp)).toFloat
    end logSumExp

    inline def -(vec2: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.saxpy(vec.length, -1.0f, vec2, 1, vec, 1)
    end -=

    inline def +(vec2: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
      dimCheck(vec, vec2)
      vec.clone.tap(_ += vec2)
    end +

    inline def +=(vec2: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.saxpy(vec.length, 1.0f, vec2, 1, vec, 1)
    end +=

    inline def +:+(d: Float): Array[Float] =
      vec.clone.tap(_ +:+= d)
    end +:+

    inline def +:+=(d: Float): Unit =
      var i: Int = 0
      while i < vec.length do
        vec(i) += d
        i += 1
      end while
    end +:+=

    inline def +(d: Float): Array[Float] =
      val out = new Array[Float](vec.length)
      val inc = FloatVector.broadcast(spf, d)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .add(inc)
          .intoArray(out, i)
        i += spfl
      end while

      while i < vec.length do
        out(i) = vec(i) + d
        i = i + 1
      end while
      out
    end +

    inline def +=(d: Float): Unit =
      val inc = FloatVector.broadcast(spf, d)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .add(inc)
          .intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    inline def -(d: Float): Array[Float] =
      val out = new Array[Float](vec.length)
      val inc = FloatVector.broadcast(spf, d)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .sub(inc)
          .intoArray(out, i)
        i += spfl
      end while

      while i < vec.length do
        out(i) = vec(i) - d
        i = i + 1
      end while
      out
    end -

    inline def -=(d: Float): Unit =
      val inc = FloatVector.broadcast(spf, d)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .sub(inc)
          .intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    inline def *:*(d: Array[Float])(using inline boundsCheck: BoundsCheck) = vec * d

    inline def *(d: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
      dimCheck(vec, d)
      val out = new Array[Float](vec.length)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .mul(FloatVector.fromArray(spf, d, i))
          .intoArray(out, i)
        i += spfl
      end while

      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *

    // inline def *=(d: Float): Unit =
    //   var i = 0
    //   while i < spf.loopBound(vec.length) do
    //     FloatVector
    //       .fromArray(spf, vec, i)
    //       .mul(d)
    //       .intoArray(vec, i)
    //     i += spfl
    //   end while

    //   while i < vec.length do
    //     vec(i) = vec(i) * d
    //     i = i + 1
    //   end while
    // end *=

    // inline def *(d: Float): Unit =
    //   vec.clone().tap(_ *= d)
    // end *

    inline def *=(d: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, d)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .mul(FloatVector.fromArray(spf, d, i))
          .intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        vec(i) = vec(i) * d(i)
        i = i + 1
      end while
    end *=

    inline def /:/(d: Array[Float])(using inline boundsCheck: BoundsCheck) = vec / d

    inline def /(d: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
      dimCheck(vec, d)
      val out = new Array[Float](vec.length)
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .div(FloatVector.fromArray(spf, d, i))
          .intoArray(out, i)
        i += spfl
      end while

      while i < vec.length do
        out(i) = vec(i) / d(i)
        i = i + 1
      end while
      out
    end /

    inline def /=(d: Float): Unit =
      blas.sscal(vec.length, 1.0f / d, vec, 1)
    end /=

    inline def /(d: Float): Array[Float] =
      vec.clone.tap(_ /= d)
    end /

    inline def *=(d: Float): Unit =
      var i = 0
      while i < spf.loopBound(vec.length) do
        FloatVector
          .fromArray(spf, vec, i)
          .mul(FloatVector.broadcast(spf, d))
          .intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        vec(i) *= d
        i += 1
      end while
    end *=

    inline def *(d: Float): Array[Float] =
      vec.clone.tap(_ *= d)
    end *

    inline def =:=(num: Float): Array[Boolean] =
      logicalFloatIdx(VectorOperators.EQ, num)

    inline def !:=(num: Float): Array[Boolean] =
      logicalFloatIdx(VectorOperators.NE, num)

    inline def <(num: Float): Array[Boolean] =
      logicalFloatIdx(VectorOperators.LT, num)

    inline def <=(num: Float): Array[Boolean] =
      logicalFloatIdx(VectorOperators.LE, num)

    inline def >(num: Float): Array[Boolean] =
      logicalFloatIdx(VectorOperators.GT, num)

    inline def >=(num: Float): Array[Boolean] =
      logicalFloatIdx(VectorOperators.GE, num)

    private inline def logicalFloatIdx(
        inline op: VectorOperators.Comparison,
        num: Float
    ): Array[Boolean] =
      val idx = new Array[Boolean](vec.length)
      var i = 0

      while i < spf.loopBound(vec.length) do
        FloatVector.fromArray(spf, vec, i).compare(op, num).intoArray(idx, i)
        i += spfl
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
    end logicalFloatIdx

    def covariance(thatVector: Array[Float]): Float =
      val μThis = vec.mean
      val μThat = thatVector.mean
      val n = vec.length
      var i = 0
      var acc = FloatVector.zero(spf)

      while i < spf.loopBound(n) do
        val v1 = FloatVector.fromArray(spf, vec, i).sub(μThis)
        val v2 = FloatVector.fromArray(spf, thatVector, i).sub(μThat)
        acc = v1.fma(v2, acc)
        i += spfl
      end while

      var cv = acc.reduceLanes(VectorOperators.ADD).toDouble
      while i < n do
        cv += (vec(i) - μThis) * (thatVector(i) - μThat)
        i += 1
      end while

      (cv / (n - 1)).toFloat
    end covariance

    inline def pearsonCorrelationCoefficient(thatVector: Array[Float])(using
        inline boundsCheck: BoundsCheck
    ): Float =
      dimCheck(vec, thatVector)
      val n = vec.length
      var i = 0

      var sum_x = 0.0
      var sum_y = 0.0
      var sum_xy = 0.0
      var sum_x2 = 0.0
      var sum_y2 = 0.0

      while i < n do
        val x = vec(i).toDouble
        val y = thatVector(i).toDouble
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = Math.fma(x, y, sum_xy)
        sum_x2 = Math.fma(x, x, sum_x2)
        sum_y2 = Math.fma(y, y, sum_y2)
        i = i + 1
      end while
      ((n * sum_xy - (sum_x * sum_y)) / Math.sqrt(
        (sum_x2 * n - sum_x * sum_x) * (sum_y2 * n - sum_y * sum_y)
      )).toFloat
    end pearsonCorrelationCoefficient

    inline def spearmansRankCorrelation(thatVector: Array[Float])(using inline boundsCheck: BoundsCheck): Float =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    inline def corr(thatVector: Array[Float])(using inline boundsCheck: BoundsCheck): Float =
      pearsonCorrelationCoefficient(thatVector)

    inline def elementRanks: Array[Float] =
      val indexed: Array[(Float, Int)] = vec.zipWithIndex
      indexed.sortInPlace()(using Ordering.by(_._1))

      val ranks: Array[Float] = new Array[Float](vec.length)
      ranks(indexed.last._2) = vec.length.toFloat
      var currentValue: Float = indexed(0)._1
      var r0: Int = 0
      var rank: Int = 1
      while rank < vec.length do
        val temp: Float = indexed(rank)._1
        val end: Int =
          if temp != currentValue then rank
          else if rank == vec.length - 1 then rank + 1
          else -1
        if end > -1 then
          val avg: Float = ((1.0 + (end + r0)) / 2.0).toFloat
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

    inline def outer(other: Array[Float])(using ClassTag[Float]): Matrix[Float] =
      val n = vec.length
      val m = other.length
      val out = new Array[Float](n * m)

      var j = 0
      while j < m do
        var i = 0
        val tmp = FloatVector.broadcast(spf, other(j))
        while i < spf.loopBound(n) do
          FloatVector.fromArray(spf, vec, i).mul(tmp).intoArray(out, j * n + i)
          i = i + spfl
        end while

        while i < n do
          out(j * n + i) = vec(i) * other(j)
          i = i + 1
        end while
        j = j + 1
      end while
      Matrix(out, (n, m))(using BoundsCheck.DoBoundsCheck.no)
    end outer

    inline def `zeroWhere!`(
        other: Array[Float],
        threshold: Float,
        inline op: ComparisonOp
    ): Unit =
      assert(vec.length == other.length)
      val zero = FloatVector.zero(spf)
      val thresh = FloatVector.broadcast(spf, threshold)
      var i = 0

      while i < spf.loopBound(vec.length) do
        val values = FloatVector.fromArray(spf, vec, i)
        val cmp = FloatVector.fromArray(spf, other, i)
        val mask = inline op match
          case ComparisonOp.LE => cmp.compare(VectorOperators.LE, thresh)
          case ComparisonOp.LT => cmp.compare(VectorOperators.LT, thresh)
          case ComparisonOp.GE => cmp.compare(VectorOperators.GE, thresh)
          case ComparisonOp.GT => cmp.compare(VectorOperators.GT, thresh)
          case ComparisonOp.EQ => cmp.compare(VectorOperators.EQ, thresh)
          case ComparisonOp.NE => cmp.compare(VectorOperators.NE, thresh)
        values.blend(zero, mask).intoArray(vec, i)
        i += spfl
      end while

      while i < vec.length do
        val hit = inline op match
          case ComparisonOp.LE => other(i) <= threshold
          case ComparisonOp.LT => other(i) < threshold
          case ComparisonOp.GE => other(i) >= threshold
          case ComparisonOp.GT => other(i) > threshold
          case ComparisonOp.EQ => other(i) == threshold
          case ComparisonOp.NE => other(i) != threshold
        if hit then vec(i) = 0.0f
        end if
        i += 1
      end while
    end `zeroWhere!`

    inline def zeroWhere(
        other: Array[Float],
        threshold: Float,
        inline op: ComparisonOp
    ): Array[Float] =
      vec.clone().tap(_.`zeroWhere!`(other, threshold, op))

  end extension
end floatarrays
