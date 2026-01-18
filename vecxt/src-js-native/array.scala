package vecxt

import scala.math.Ordering
import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixInstance.*
import vecxt.matrix.Matrix

// These use project panama (SIMD) on the JVM, so need own JS native implementation
object JsNativeDoubleArrays:

  def linspace(a: Double, b: Double, length: Int = 100): Array[Double] =
    val increment = (b - a) / (length - 1)
    Array.tabulate[Double](length)(i => a + increment * i)
  end linspace

  extension (d: Double)
    inline def /(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d / arr(i)
        i = i + 1
      end while
      out
    end /

    inline def +(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d + arr(i)
        i = i + 1
      end while
      out
    end +

    inline def -(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d - arr(i)
        i = i + 1
      end while
      out
    end -

    inline def *(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d * arr(i)
        i = i + 1
      end while
      out
    end *

  end extension

  extension (m: Matrix[Double])
    // TODO: SIMD
    inline def *:*(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val newArr = Array.ofDim[Double](m.rows * m.cols)
        var i = 0
        while i < newArr.length do
          newArr(i) = if bmat.raw(i) then m.raw(i) else 0.0
          i += 1
        end while
        Matrix[Double](newArr, (m.rows, m.cols))
      else ???
      end if
    end *:*

    inline def +=(arr: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =

      if boundsCheck then assert(arr.length == m.cols, s"Array length ${arr.length} != expected ${m.cols}")
      end if

      var i = 0
      while i < m.rows do
        var j = 0
        while j < m.cols do
          m(i, j) = m(i, j) + arr(j)
          j += 1
        end while
        i += 1
      end while

    end +=

    inline def +=(n: Double): Unit =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.+=(m.raw)(n)
      else
        // Cache-friendly fallback: iterate with smallest stride in inner loop
        if m.rowStride <= m.colStride then
          // Row stride is smaller, so iterate rows in inner loop
          var j = 0
          while j < m.cols do
            var i = 0
            while i < m.rows do
              m(i, j) = n + m(i, j)
              i += 1
            end while
            j += 1
          end while
        else
          // Column stride is smaller, so iterate columns in inner loop
          var i = 0
          while i < m.rows do
            var j = 0
            while j < m.cols do
              m(i, j) = n + m(i, j)
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end +=

    inline def >=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](m.raw >= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def >(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](m.raw > d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
      end if
    end >

    inline def <=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](m.raw <= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
      end if
    end <=

    inline def <(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](m.raw < d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
  end extension

  // extension [@specialized(Double, Int) A: Numeric](m: Matrix[A])
  //   inline def >=(d: A): Matrix[Boolean] =
  //     Matrix[Boolean](m.raw >= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

  //   inline def >(d: A): Matrix[Boolean] =
  //     Matrix[Boolean](m.raw > d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

  //   inline def <=(d: A): Matrix[Boolean] =
  //     Matrix[Boolean](m.raw <= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

  //   inline def <(d: A): Matrix[Boolean] =
  //     Matrix[Boolean](m.raw < d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
  // end extension

  extension (vec: Array[Double])

    inline def clampMin(min: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.max(vec(i), min)
        i += 1
      end while
      res
    end clampMin

    inline def `clampMin!`(min: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.max(vec(i), min)
        i += 1
      end while
    end `clampMin!`

    inline def maxClamp(max: Double): Array[Double] = clampMax(max)

    inline def minClamp(min: Double): Array[Double] = clampMin(min)

    inline def clampMax(max: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.min(vec(i), max)
        i += 1
      end while
      res
    end clampMax

    inline def `clampMax!`(max: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.min(vec(i), max)
        i += 1
      end while
    end `clampMax!`

    inline def clamp(min: Double, max: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.min(Math.max(vec(i), min), max)
        i += 1
      end while
      res
    end clamp
    inline def `clamp!`(min: Double, max: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.min(Math.max(vec(i), min), max)
        i += 1
      end while
    end `clamp!`

    inline def argmax: Int =
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

    inline def argmin: Int =
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

    inline def productSIMD: Double = vecxt.arrays.product(vec)

    inline def sumSIMD: Double = vecxt.arrays.sum(vec)

    inline def `**!`(power: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.pow(vec(i), power)
        i += 1
      end while
    end `**!`

    inline def **(power: Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.pow(vec(i), power)
        i += 1
      end while
      newVec
    end **

    inline def `fma!`(multiply: Double, add: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) * multiply + add
        i += 1
      end while
    end `fma!`

    inline def `fma`(multiply: Double, add: Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = vec(i) * multiply + add
        i += 1
      end while
      newVec
    end `fma`

    inline def exp: Array[Double] =
      applyUnaryOp(Math.exp)

    inline def `exp!`: Unit =
      applyUnaryOpInPlace(Math.exp)

    inline def log: Array[Double] =
      applyUnaryOp(Math.log)

    inline def `log!`: Unit =
      applyUnaryOpInPlace(Math.log)

    inline def sqrt: Array[Double] =
      applyUnaryOp(Math.sqrt)

    inline def `sqrt!`: Unit =
      applyUnaryOpInPlace(Math.sqrt)

    inline def cbrt: Array[Double] =
      applyUnaryOp(Math.cbrt)

    inline def `cbrt!`: Unit =
      applyUnaryOpInPlace(Math.cbrt)

    inline def sin: Array[Double] =
      applyUnaryOp(Math.sin)

    inline def `sin!`: Unit =
      applyUnaryOpInPlace(Math.sin)

    inline def cos: Array[Double] =
      applyUnaryOp(Math.cos)

    inline def `cos!`: Unit =
      applyUnaryOpInPlace(Math.cos)

    inline def tan: Array[Double] =
      applyUnaryOp(Math.tan)

    inline def `tan!`: Unit =
      applyUnaryOpInPlace(Math.tan)

    inline def asin: Array[Double] =
      applyUnaryOp(Math.asin)

    inline def `asin!`: Unit =
      applyUnaryOpInPlace(Math.asin)

    inline def - : Array[Double] =
      applyUnaryOp(-_)

    inline def `-!`: Unit =
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

    inline def /(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
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

    inline def productExceptSelf: Array[Double] =
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
    inline def logSumExp: Double =
      val maxVal = vec.max
      val sumExp = vec.map(x => Math.exp(x - maxVal)).sum
      maxVal + Math.log(sumExp)
    end logSumExp

    inline def *(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
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

    inline def *=(d: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, d)
      val n = vec.length

      var i = 0
      while i < n do
        vec(i) = vec(i) * d(i)
        i += 1
      end while
    end *=

    inline def outer(other: Array[Double])(using ClassTag[Double]): Matrix[Double] =
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
      Matrix[Double](out, (n, m))(using BoundsCheck.DoBoundsCheck.no)
    end outer

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

  extension (vec: Array[Int])

    inline def increments: Array[Int] =
      val n = vec.length
      val idx = Array.ofDim[Int](vec.length)

      var i = 1
      while i < n do

        idx(i) = vec(i) - vec(i - 1)
        // println(s"i: $i || vec(i): ${vec(i)} || vec(i - 1): ${vec(i - 1)} || idx(i): ${idx(i)}")
        i = i + 1
      end while
      idx(0) = vec(0)
      idx
    end increments

    inline def -(other: Array[Int])(using inline boundsCheck: BoundsCheck): Array[Int] =
      dimCheck(vec, other)
      val n = vec.length
      val res = Array.fill(n)(0)

      var i = 0
      while i < n do
        res(i) = vec(i) - other(i)
        i = i + 1
      end while
      res
    end -

    inline def +(other: Array[Int])(using inline boundsCheck: BoundsCheck): Array[Int] =
      dimCheck(vec, other)

      val n = vec.length
      val res = Array.fill(n)(0)

      var i = 0
      while i < n do
        res(i) = vec(i) + other(i)
        i = i + 1
      end while
      res
    end +

    inline def dot(other: Array[Int])(using inline boundsCheck: BoundsCheck): Int =
      dimCheck(vec, other)
      val n = vec.length
      var sum = 0

      var i = 0
      while i < n do
        sum += vec(i) * other(i)
        i = i + 1
      end while
      sum
    end dot

    inline def <(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Int): Array[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Int, Int) => Boolean,
        inline num: Int
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

  //   extension [@specialized(Double, Int) A: Numeric](vec: Array[A])

  //   inline def <(num: A)(using inline o: Ordering[A]): Array[Boolean] =
  //     logicalIdx((a: A, b: A) => o.lt(a, b), num)

  //   inline def <=(num: A)(using inline o: Ordering[A]): Array[Boolean] =
  //     logicalIdx((a: A, b: A) => o.lteq(a, b), num)

  //   inline def >(num: A)(using inline o: Ordering[A]): Array[Boolean] =
  //     logicalIdx((a: A, b: A) => o.gt(a, b), num)

  //   inline def >=(num: A)(using inline o: Ordering[A]): Array[Boolean] =
  //     logicalIdx((a: A, b: A) => o.gteq(a, b), num)

  //   inline def logicalIdx(
  //       inline op: (A, A) => Boolean,
  //       inline num: A
  //   ): Array[Boolean] =
  //     val n = vec.length
  //     val idx = Array.fill(n)(false)

  //     var i = 0
  //     while i < n do
  //       if op(vec(i), num) then idx(i) = true
  //       end if
  //       i = i + 1
  //     end while
  //     idx
  //   end logicalIdx
  // end extension

end JsNativeDoubleArrays
