package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import all.*

import jdk.incubator.vector.*

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas

object JvmDoubleMatrix:

  final val sp_int_doubleLanes =
    VectorSpecies.of(java.lang.Integer.TYPE, VectorShape.forBitSize(vecxt.arrays.spdl * Integer.SIZE));

  extension (m: Matrix[Double]) // inline def /(n: Double): Matrix[Double] =
    //   Matrix(vecxt.arrays./(m.raw)(n), m.shape)(using BoundsCheck.DoBoundsCheck.no)

    // TODO check whether this work with flexible memory layout patterns
    inline def `matmulInPlace!`(b: Matrix[Double], c: Matrix[Double], alpha: Double = 1.0, beta: Double = 0.0)(using
        inline boundsCheck: BoundsCheck
    ): Unit =
      dimMatCheck(m, b)

      val mStr = if m.isDenseColMajor then "N" else "T"
      val bStr = if b.isDenseColMajor then "N" else "T"
      val lda = if m.isDenseColMajor then m.rows else m.cols
      val ldb = if b.isDenseColMajor then b.rows else b.cols

      if m.hasSimpleContiguousMemoryLayout && b.hasSimpleContiguousMemoryLayout then

        blas.dgemm(
          mStr,
          bStr,
          m.rows,
          b.cols,
          m.cols,
          alpha,
          m.raw,
          lda,
          b.raw,
          ldb,
          beta,
          c.raw,
          m.rows
        )
      else
        //   if m.isColMajor && b.isColMajor then

        //   blas.dgemm(
        //     mStr,
        //     bStr,
        //     m.rows,
        //     b.cols,
        //     m.cols,
        //     alpha,
        //     m.raw,
        //     m.offset,
        //     lda,
        //     b.raw,
        //     b.offset,
        //     ldb,
        //     beta,
        //     c.raw,
        //     0,
        //     m.rows
        //   )
        // else
        // I don't think this is implementable with traditional BLAS
        ???
      end if

    end `matmulInPlace!`

    // TODO: SIMD
    inline def *:*(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        val newArr = Array.fill[Double](m.rows * m.cols)(0.0)
        var i = 0
        while i < newArr.length do
          newArr(i) = if bmat.raw(i) then m.raw(i) else 0.0
          i += 1
        end while
        Matrix(newArr, m.rows, m.cols)
      else ???
      end if
    end *:*

    inline def *:*=(bmat: Matrix[Boolean])(using inline boundsCheck: BoundsCheck): Unit =
      sameDimMatCheck(m, bmat)
      if sameDenseElementWiseMemoryLayoutCheck(m, bmat) then
        var i = 0
        while i < m.raw.length do
          m.raw.update(i, (if bmat.raw(i) then 1.0 else 0.0) * m.raw(i))
          i += 1
        end while
      else ???
      end if
    end *:*=

    // inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    // inline def *:*=(d: Double): Unit = m.raw.multInPlace(d)

    // TODO: Dim check

    inline def *(vec: Array[Double], alpha: Double = 1.0, beta: Double = 1.0)(using
        inline boundsCheck: BoundsCheck
    ): Array[Double] =

      if m.isDenseColMajor then
        require(vec.length == m.cols, s"Vector length ${vec.length} != expected ${m.cols}")
        val newArr = Array.ofDim[Double](m.rows)
        val out = Array.fill(m.rows)(0.0)

        blas.dgemv(
          "N",
          m.rows,
          m.cols,
          alpha,
          m.raw,
          m.rows,
          vec,
          1,
          beta,
          newArr,
          1
        )

        newArr
      else ???
    end *

    inline def >=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.arrays.>=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def >(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.arrays.>(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def <=(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.arrays.<=(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    inline def <(d: Double): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        Matrix[Boolean](vecxt.arrays.<(m.raw)(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???

    def +=(n: Double): Unit =
      import vecxt.BoundsCheck.DoBoundsCheck.no
      if m.hasSimpleContiguousMemoryLayout then vecxt.arrays.+=(m.raw)(n)
      else
        // println(s" .offset: ${m.offset}, m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
        // Cache-friendly fallback: iterate with smallest stride in inner loop
        // (m.offset + row * m.rowStride + col * m.colStride
        if m.rowStride <= m.colStride then
          // Row stride is smaller, so iterate rows in inner loop
          val rowStrides = IntVector.zero(sp_int_doubleLanes).addIndex(m.rowStride).toArray
          // println(m.offset)
          // println(s"colStrides: ${rowStrides.mkString(", ")}")
          // println(s"m.raw: ${m.raw.mkString(", ")}")
          // println(s"m.rows: ${m.rows}, m.cols: ${m.cols}")
          // println(s"m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
          var j = 0
          while j < m.cols do
            var i = 0
            var blockIndex = m.offset + j * m.colStride
            val upperBound = sp_int_doubleLanes.loopBound(m.rows)
            while i < upperBound do
              val iBlockIndex = blockIndex + i * m.rowStride
              DoubleVector
                .fromArray(vecxt.arrays.spd, m.raw, iBlockIndex, rowStrides, 0)
                .add(n)
                .intoArray(m.raw, iBlockIndex, rowStrides, 0)
              i += sp_int_doubleLanes.length
            end while
            while i < m.rows do
              m.elementIndex(i, j)(using BoundsCheck.DoBoundsCheck.yes)
              m(i, j) = n + m(i, j)
              i += 1
            end while

            j += 1
          end while
        else
          // Column stride is smaller, so iterate columns in inner loop
          val colStrides = IntVector.zero(sp_int_doubleLanes).addIndex(m.colStride).toArray
          // println(m.offset)
          // println(s"colStrides: ${colStrides.mkString(", ")}")
          // println(s"m.raw: ${m.raw.mkString(", ")}")
          // println(s"m.rows: ${m.rows}, m.cols: ${m.cols}")
          // println(s"m.rowStride: ${m.rowStride}, m.colStride: ${m.colStride}")
          var i = 0
          while i < m.rows do
            var j = 0
            val upperBound = sp_int_doubleLanes.loopBound(m.cols)

            var blockIndex = m.offset + i * m.rowStride
            while j < upperBound do
              val jblockIndex = blockIndex + j * m.colStride
              DoubleVector
                .fromArray(vecxt.arrays.spd, m.raw, jblockIndex, colStrides, 0)
                .add(n)
                .intoArray(m.raw, jblockIndex, colStrides, 0)

              j += sp_int_doubleLanes.length
            end while

            while j < m.cols do
              m.elementIndex(i, j)(using BoundsCheck.DoBoundsCheck.yes)
              m(i, j) = n + m(i, j)
              j += 1
            end while
            i += 1
          end while
        end if
      end if

    end +=

  end extension

end JvmDoubleMatrix

object NativeDoubleMatrix:

end NativeDoubleMatrix
