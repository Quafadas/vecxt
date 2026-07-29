package vecxt

import scala.reflect.ClassTag
import scala.scalanative.unsafe.*

import org.ekrich.blas.unsafe.*

import vecxt.BooleanArrays.trues

import vecxt.matrix.Matrix

object doublearrays:

  def linspace(a: Double, b: Double, length: Int = 100): Array[Double] =
    val increment = (b - a) / (length - 1)
    Array.tabulate[Double](length)(i => a + increment * i)
  end linspace

  extension (d: Double)
    def /(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d / arr(i)
        i = i + 1
      end while
      out
    end /

    def +(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d + arr(i)
        i = i + 1
      end while
      out
    end +

    def -(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d - arr(i)
        i = i + 1
      end while
      out
    end -

    def *(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d * arr(i)
        i = i + 1
      end while
      out
    end *

  end extension

  extension (vec: Array[Double])

    def clampMin(min: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.max(vec(i), min)
        i += 1
      end while
      res
    end clampMin

    def `clampMin!`(min: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.max(vec(i), min)
        i += 1
      end while
    end `clampMin!`

    def maxClamp(max: Double): Array[Double] = clampMax(max)

    def minClamp(min: Double): Array[Double] = clampMin(min)

    def clampMax(max: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.min(vec(i), max)
        i += 1
      end while
      res
    end clampMax

    def `clampMax!`(max: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.min(vec(i), max)
        i += 1
      end while
    end `clampMax!`

    def clamp(min: Double, max: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.min(Math.max(vec(i), min), max)
        i += 1
      end while
      res
    end clamp
    def `clamp!`(min: Double, max: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.min(Math.max(vec(i), min), max)
        i += 1
      end while
    end `clamp!`

    def argmax: Int =
      val n = vec.length
      if n == 0 then -1 // Handle empty array case
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

    def argmin: Int =
      val n = vec.length
      if n == 0 then -1 // Handle empty array case
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

    def `**!`(power: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.pow(vec(i), power)
        i += 1
      end while
    end `**!`

    def **(power: Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.pow(vec(i), power)
        i += 1
      end while
      newVec
    end **

    def `fma!`(multiply: Double, add: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) * multiply + add
        i += 1
      end while
    end `fma!`

    def `fma`(multiply: Double, add: Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = vec(i) * multiply + add
        i += 1
      end while
      newVec
    end `fma`

    def exp: Array[Double] =
      applyUnaryOp(Math.exp)

    def `exp!`: Unit =
      applyUnaryOpInPlace(Math.exp)

    def log: Array[Double] =
      applyUnaryOp(Math.log)

    def `log!`: Unit =
      applyUnaryOpInPlace(Math.log)

    def sqrt: Array[Double] =
      applyUnaryOp(Math.sqrt)

    def `sqrt!`: Unit =
      applyUnaryOpInPlace(Math.sqrt)

    def cbrt: Array[Double] =
      applyUnaryOp(Math.cbrt)

    def `cbrt!`: Unit =
      applyUnaryOpInPlace(Math.cbrt)

    def sin: Array[Double] =
      applyUnaryOp(Math.sin)

    def `sin!`: Unit =
      applyUnaryOpInPlace(Math.sin)

    def cos: Array[Double] =
      applyUnaryOp(Math.cos)

    def `cos!`: Unit =
      applyUnaryOpInPlace(Math.cos)

    def tan: Array[Double] =
      applyUnaryOp(Math.tan)

    def `tan!`: Unit =
      applyUnaryOpInPlace(Math.tan)

    def asin: Array[Double] =
      applyUnaryOp(Math.asin)

    def `asin!`: Unit =
      applyUnaryOpInPlace(Math.asin)

    def - : Array[Double] =
      applyUnaryOp(-_)

    def `-!`: Unit =
      applyUnaryOpInPlace(-_)

    private inline def applyUnaryOp(inline op: Double => Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = op(vec(i))
        i += 1
      end while
      newVec
    end applyUnaryOp

    private inline def applyUnaryOpInPlace(inline op: Double => Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = op(vec(i))
        i += 1
      end while
    end applyUnaryOpInPlace

    def /(d: Array[Double]): Array[Double] =
      dimCheck(vec, d)
      val n = vec.length
      val res = Array.ofDim[Double](n)
      var i = 0
      while i < n do
        res(i) = vec(i) / d(i)
        i += 1
      end while
      res
    end /

    def productExceptSelf: Array[Double] =
      val n = vec.length
      val left = Array.ofDim[Double](n)
      val right = Array.ofDim[Double](n)
      val result = Array.ofDim[Double](n)

      left(0) = 1.0
      right(n - 1) = 1.0

      var i = 1
      while i < n do
        left(i) = vec(i - 1) * left(i - 1)
        i += 1
      end while

      i = n - 2
      while i >= 0 do
        right(i) = vec(i + 1) * right(i + 1)
        i -= 1
      end while

      i = 0
      while i < n do
        result(i) = left(i) * right(i)
        i += 1
      end while

      result
    end productExceptSelf

    /** The formula for the logarithm of the sum of exponentials is:
      *
      * logSumExp(x) = log(sum(exp(x_i))) for i = 1 to n
      *
      * This is computed in a numerically stable way by subtracting the maximum value in the array before taking the
      * exponentials:
      *
      * logSumExp(x) = max(x) + log(sum(exp(x_i - max(x)))) for i = 1 to n
      */
    def logSumExp: Double =
      val maxVal = vec.max
      val sumExp = vec.map(x => Math.exp(x - maxVal)).sum
      maxVal + Math.log(sumExp)
    end logSumExp

    def *(d: Array[Double]): Array[Double] =
      dimCheck(vec, d)
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = vec(i) * d(i)
        i += 1
      end while
      res
    end *

    def *:*(d: Array[Double]): Array[Double] = vec.*(d)

    def *:*=(d: Array[Double]): Unit = vec.*=(d)

    def *=(d: Array[Double]): Unit =
      dimCheck(vec, d)
      val n = vec.length

      var i = 0
      while i < n do
        vec(i) = vec(i) * d(i)
        i += 1
      end while
    end *=

    def outer(other: Array[Double])(using ClassTag[Double]): Matrix[Double] =
      val n = vec.length
      val m = other.length
      val out: Array[Double] = Array.ofDim[Double](n * m)

      var i = 0
      while i < n do
        var j = 0
        while j < m do
          out(j * n + i) = vec(i) * other(j)
          j = j + 1
        end while
        i = i + 1
      end while
      Matrix[Double](out, (n, m))
    end outer

    def <(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    def <=(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    def >(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    def >=(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Double, Double) => Boolean,
        inline num: Double
    ): Array[Boolean] =
      val n = vec.length
      val idx = Array.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx
  end extension

  extension (vec: Array[Boolean])
    // inline def trues: Int =
    //   var sum = 0
    //   for i <- 0 until vec.length do if vec(i) then sum = sum + 1
    //   end for
    //   sum
    // end trues

    def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) && thatIdx(i)
      end for
      result
    end &&

    def ||(thatIdx: Array[Boolean]): Array[Boolean] =
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

  extension [A: ClassTag](vec: Array[A])

    def apply(index: Array[Boolean]): Array[A] =
      val truely = index.trues
      val newVec = Array.ofDim[A](truely)
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

  extension (vec: Array[Double])

    def apply(index: Array[Boolean]) =
      val trues = index.trues
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

    def minSIMD: Double =
      var i = 0
      var acc = Double.PositiveInfinity
      while i < vec.length do
        val v = vec(i)
        if v < acc then acc = v
        end if
        i += 1
      end while
      acc
    end minSIMD

    def maxSIMD: Double =
      var i = 0
      var acc = Double.NegativeInfinity
      while i < vec.length do
        val v = vec(i)
        if v > acc then acc = v
        end if
        i += 1
      end while
      acc
    end maxSIMD

    def product: Double =
      var sum = 1.0
      var i = 0;
      while i < vec.length do
        sum *= vec(i)
        i = i + 1
      end while
      sum
    end product

    def productSIMD: Double = product

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

    def pearsonCorrelationCoefficient(thatVector: Array[Double]): Double =
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

    def spearmansRankCorrelation(thatVector: Array[Double]): Double =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    // An alias - pearson is the most commonly requested type of correlation
    def corr(thatVector: Array[Double]): Double =
      pearsonCorrelationCoefficient(thatVector)

    def elementRanks: Array[Double] =
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

    def variance: Double = variance(VarianceMode.Population)

    def variance(mode: VarianceMode): Double =
      meanAndVariance(mode).variance
    end variance

    def std: Double = std(VarianceMode.Population)

    def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    def stdDev: Double = stdDev(VarianceMode.Population)

    def stdDev(mode: VarianceMode): Double = std(mode)

    def meanAndVariance: (mean: Double, variance: Double) =
      meanAndVariance(VarianceMode.Population)

    def meanAndVariance(mode: VarianceMode): (mean: Double, variance: Double) =
      var mean = 0.0
      var m2 = 0.0
      var i = 0
      while i < vec.length do
        val n = i + 1
        val delta = vec(i) - mean
        mean += delta / n
        val delta2 = vec(i) - mean
        m2 += delta * delta2
        i += 1
      end while

      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      (mean, m2 / denom)
    end meanAndVariance

    def mean: Double = vec.sumSIMD / vec.length

    def sumSIMD: Double = sum

    def sum: Double =
      var sum = 0.0
      var i = 0;
      while i < vec.length do
        sum = sum + vec(i)
        i = i + 1
      end while
      sum
    end sum

    def `cumsum!`: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    def cumsum: Array[Double] =
      val out = vec.clone
      out.`cumsum!`
      out
    end cumsum

    def unary_- : Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = -vec(i)
        i += 1
      end while
      newVec
    end unary_-

    def norm: Double = blas.cblas_dnrm2(vec.length, vec.at(0), 1)

    def dot(v1: Array[Double]): Double =
      dimCheck(vec, v1)
      blas.cblas_ddot(vec.length, vec.at(0), 1, v1.at(0), 1)
    end dot

    def -(vec2: Array[Double]): Array[Double] =
      val out = vec.clone
      out -= vec2
      out
    end -

    def -=(vec2: Array[Double]): Unit =
      dimCheck(vec, vec2)
      blas.cblas_daxpy(vec.length, -1.0, vec2.at(0), 1, vec.at(0), 1)
    end -=

    def +(vec2: Array[Double]): Array[Double] =
      val out = vec.clone
      out += vec2
      out
    end +

    def +=(vec2: Array[Double]): Unit =
      dimCheck(vec, vec2)
      blas.cblas_daxpy(vec.length, 1.0, vec2.at(0), 1, vec.at(0), 1)
    end +=

    def +:+(d: Double): Array[Double] =
      val out = vec.clone
      out +:+= d
      out
    end +:+

    def +:+=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +:+=

    def add(d: Array[Double]): Array[Double] = vec + d
    def multInPlace(d: Double): Unit = vec *= d

    def -(d: Double): Array[Double] =
      val out = vec.clone()
      out -= d
      out
    end -

    def -=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    def +(d: Double): Array[Double] =
      val out = vec.clone()
      out += d
      out
    end +

    def +=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    def *=(d: Double): Unit =
      blas.cblas_dscal(vec.length, d, vec.at(0), 1)
    end *=

    def *(d: Double): Array[Double] =
      val out = vec.clone
      out *= d
      out
    end *

    def /=(d: Double): Unit =
      blas.cblas_dscal(vec.length, 1 / d, vec.at(0), 1)

    def /(d: Double) =
      val out = vec.clone
      out /= d
      out
    end /

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

    inline def `zeroWhere!`(
        other: Array[Double],
        threshold: Double,
        inline op: ComparisonOp
    ): Unit =
      assert(vec.length == other.length)
      var i = 0
      while i < vec.length do
        val hit = inline op match
          case ComparisonOp.LE => other(i) <= threshold
          case ComparisonOp.LT => other(i) < threshold
          case ComparisonOp.GE => other(i) >= threshold
          case ComparisonOp.GT => other(i) > threshold
          case ComparisonOp.EQ => other(i) == threshold
          case ComparisonOp.NE => other(i) != threshold
        if hit then vec(i) = 0.0
        end if
        i += 1
      end while
    end `zeroWhere!`

    inline def zeroWhere(
        other: Array[Double],
        threshold: Double,
        inline op: ComparisonOp
    ): Array[Double] =
      val out = vec.clone()
      out.`zeroWhere!`(other, threshold, op)
      out
    end zeroWhere

  end extension

end doublearrays
