package vecxt

import vecxt.matrix.Matrix
import vecxt.BoundsCheck.BoundsCheck

import scala.math.Ordering

import narr.*
import scala.reflect.ClassTag
import scala.util.chaining.*

object JsNativeBooleanArrays:

  extension (vec: NArray[Boolean])

    inline def all = vec.forall(identity)

    inline def any: Boolean =
      var i = 0
      var any = false
      while i < vec.length && any == false do
        if vec(i) then any = true
        end if
        i += 1
      end while
      any
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
  end extension
end JsNativeBooleanArrays

// These use project panama (SIMD) on the JVM, so need own JS native implementation
object JsNativeDoubleArrays:

  extension (m: Matrix[Double])
    inline def >=(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw >= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def >(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw > d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw <= d, m.shape)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Double): Matrix[Boolean] =
      Matrix[Boolean](m.raw < d, m.shape)(using BoundsCheck.DoBoundsCheck.no)
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

  extension (vec: NArray[Double])

    inline def exp: NArray[Double] =
      applyUnaryOp(Math.exp)

    inline def `exp!`: Unit =
      applyUnaryOpInPlace(Math.exp)

    inline def log: NArray[Double] =
      applyUnaryOp(Math.log)

    inline def `log!`: Unit =
      applyUnaryOpInPlace(Math.log)

    inline def sqrt: NArray[Double] =
      applyUnaryOp(Math.sqrt)

    inline def `sqrt!`: Unit =
      applyUnaryOpInPlace(Math.sqrt)

    inline def cbrt: NArray[Double] =
      applyUnaryOp(Math.cbrt)

    inline def `cbrt!`: Unit =
      applyUnaryOpInPlace(Math.cbrt)

    inline def sin: NArray[Double] =
      applyUnaryOp(Math.sin)

    inline def `sin!`: Unit =
      applyUnaryOpInPlace(Math.sin)

    inline def cos: NArray[Double] =
      applyUnaryOp(Math.cos)

    inline def `cos!`: Unit =
      applyUnaryOpInPlace(Math.cos)

    inline def tan: NArray[Double] =
      applyUnaryOp(Math.tan)

    inline def `tan!`: Unit =
      applyUnaryOpInPlace(Math.tan)

    inline def asin: NArray[Double] =
      applyUnaryOp(Math.asin)

    inline def `asin!`: Unit =
      applyUnaryOpInPlace(Math.asin)

    private inline def applyUnaryOp(inline op: Double => Double): NArray[Double] =
      val newVec = NArray.ofSize[Double](vec.length)
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

    inline def /(d: NArray[Double])(using inline boundsCheck: BoundsCheck): NArray[Double] =
      dimCheck(vec, d)
      val n = vec.length
      val res = NArray.ofSize[Double](n)
      var i = 0
      while i < n do
        res(i) = vec(i) / d(i)
        i += 1
      end while
      res
    end /

    inline def *(d: NArray[Double])(using inline boundsCheck: BoundsCheck): NArray[Double] =
      dimCheck(vec, d)
      val n = vec.length
      val res = NArray.ofSize[Double](n)

      var i = 0
      while i < n do
        res(i) = vec(i) * d(i)
        i += 1
      end while
      res
    end *

    inline def outer(other: NArray[Double])(using ClassTag[Double]): Matrix[Double] =
      val n = vec.length
      val m = other.length
      val out: NArray[Double] = NArray.ofSize[Double](n * m)

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

    inline def <(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Double): NArray[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Double, Double) => Boolean,
        inline num: Double
    ): NArray[Boolean] =
      val n = vec.length
      val idx = NArray.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx
  end extension

  extension (vec: NArray[Int])

    inline def increments: NArray[Int] =
      val n = vec.length
      val idx = NArray.ofSize[Int](vec.length)

      var i = 1
      while i < n do

        idx(i) = vec(i) - vec(i - 1)
        // println(s"i: $i || vec(i): ${vec(i)} || vec(i - 1): ${vec(i - 1)} || idx(i): ${idx(i)}")
        i = i + 1
      end while
      idx(0) = vec(0)
      idx
    end increments

    inline def -(other: NArray[Int])(using inline boundsCheck: BoundsCheck): NArray[Int] =
      dimCheck(vec, other)
      val n = vec.length
      val res = NArray.fill(n)(0)

      var i = 0
      while i < n do
        res(i) = vec(i) - other(i)
        i = i + 1
      end while
      res
    end -

    inline def +(other: NArray[Int])(using inline boundsCheck: BoundsCheck): NArray[Int] =
      dimCheck(vec, other)

      val n = vec.length
      val res = NArray.fill(n)(0)

      var i = 0
      while i < n do
        res(i) = vec(i) + other(i)
        i = i + 1
      end while
      res
    end +

    inline def dot(other: NArray[Int])(using inline boundsCheck: BoundsCheck): Int =
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

    inline def <(num: Int): NArray[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Int): NArray[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Int): NArray[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Int): NArray[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Int, Int) => Boolean,
        inline num: Int
    ): NArray[Boolean] =
      val n = vec.length
      val idx = NArray.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx
  end extension

  //   extension [@specialized(Double, Int) A: Numeric](vec: NArray[A])

  //   inline def <(num: A)(using inline o: Ordering[A]): NArray[Boolean] =
  //     logicalIdx((a: A, b: A) => o.lt(a, b), num)

  //   inline def <=(num: A)(using inline o: Ordering[A]): NArray[Boolean] =
  //     logicalIdx((a: A, b: A) => o.lteq(a, b), num)

  //   inline def >(num: A)(using inline o: Ordering[A]): NArray[Boolean] =
  //     logicalIdx((a: A, b: A) => o.gt(a, b), num)

  //   inline def >=(num: A)(using inline o: Ordering[A]): NArray[Boolean] =
  //     logicalIdx((a: A, b: A) => o.gteq(a, b), num)

  //   inline def logicalIdx(
  //       inline op: (A, A) => Boolean,
  //       inline num: A
  //   ): NArray[Boolean] =
  //     val n = vec.length
  //     val idx = NArray.fill(n)(false)

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
