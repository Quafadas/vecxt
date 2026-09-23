package vecxt

import vecxt.matrix.*

/** Logical operations on `Matrix[Boolean]`, mirroring [[BooleanArrays]].
  *
  * Each op has the same two branches as the other matrix element-wise ops: when the backing array holds exactly the
  * matrix's elements in order, it delegates to the (SIMD on the JVM) array op and reuses `m.layout`; otherwise it walks
  * the view through `linearIndex`, and any new result is dense column-major.
  */
object BooleanMatrix:

  extension (m: Matrix[Boolean])

    /** True if every element of the matrix is true. True for an empty matrix. */
    def allTrue: Boolean =
      if m.hasSimpleContiguousMemoryLayout then BooleanArrays.allTrue(m.raw)
      else
        var out = true
        var j = 0
        while out && j < m.cols do
          var i = 0
          while out && i < m.rows do
            if !m.raw(m.layout.linearIndex(i, j)) then out = false
            end if
            i += 1
          end while
          j += 1
        end while
        out
    end allTrue

    /** True if at least one element of the matrix is true. */
    def any: Boolean =
      if m.hasSimpleContiguousMemoryLayout then BooleanArrays.any(m.raw)
      else
        var out = false
        var j = 0
        while !out && j < m.cols do
          var i = 0
          while !out && i < m.rows do
            if m.raw(m.layout.linearIndex(i, j)) then out = true
            end if
            i += 1
          end while
          j += 1
        end while
        out
    end any

    /** The number of true elements in the matrix. */
    def trues: Int =
      if m.hasSimpleContiguousMemoryLayout then BooleanArrays.trues(m.raw)
      else
        var sum = 0
        m.layout.foreach2D { (i, j) =>
          if m.raw(m.layout.linearIndex(i, j)) then sum += 1
          end if
        }
        sum
    end trues

    /** Element-wise negation, returning a new matrix. */
    def not: Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then Matrix[Boolean](BooleanArrays.not(m.raw), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          newArr(i + j * m.rows) = !m.raw(m.layout.linearIndex(i, j))
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
    end not

    /** Element-wise negation in place, writing through to the backing array (so any other view sharing it sees the
      * change).
      *
      * Throws [[UnsupportedLayoutException]] for a view whose strides alias one backing element to several positions
      * (e.g. a broadcast with a zero stride): each alias would flip the same element again, so the result would depend
      * on how many times it is repeated.
      */
    def `not!`: Unit =
      if m.hasSimpleContiguousMemoryLayout then BooleanArrays.`not!`(m.raw)
      else
        if (m.rowStride == 0 && m.rows > 1) || (m.colStride == 0 && m.cols > 1) then
          throw UnsupportedLayoutException(
            s"not! cannot be applied in place to a view that aliases elements, got layout: ${m.layoutString}"
          )
        end if
        m.layout.foreach2D { (i, j) =>
          val idx = m.layout.linearIndex(i, j)
          m.raw(idx) = !m.raw(idx)
        }
    end `not!`

    /** Element-wise logical and. Both matrices must have the same shape. */
    def &&(that: Matrix[Boolean]): Matrix[Boolean] =
      sameDimMatCheck(m, that)
      if sameDenseElementWiseMemoryLayoutCheck(m, that) then
        Matrix[Boolean](BooleanArrayOps.and(m.raw, that.raw), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          newArr(i + j * m.rows) = m.raw(m.layout.linearIndex(i, j)) && that.raw(that.layout.linearIndex(i, j))
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
      end if
    end &&

    /** Element-wise logical or. Both matrices must have the same shape. */
    def ||(that: Matrix[Boolean]): Matrix[Boolean] =
      sameDimMatCheck(m, that)
      if sameDenseElementWiseMemoryLayoutCheck(m, that) then
        Matrix[Boolean](BooleanArrayOps.or(m.raw, that.raw), m.layout)
      else
        val newArr = Array.ofDim[Boolean](m.numel)
        m.layout.foreach2D { (i, j) =>
          newArr(i + j * m.rows) = m.raw(m.layout.linearIndex(i, j)) || that.raw(that.layout.linearIndex(i, j))
        }
        Matrix[Boolean](newArr, m.rows, m.cols)
      end if
    end ||
  end extension
end BooleanMatrix

/** The array `&&` / `||` live in `BooleanArrays` on the JVM but in `arrayUtil` on JS and Native, so resolve them
  * through `all` rather than naming either. Kept outside [[BooleanMatrix]], whose own `&&` / `||` would shadow them.
  */
private object BooleanArrayOps:
  import vecxt.all.*
  def and(a: Array[Boolean], b: Array[Boolean]): Array[Boolean] = a && b
  def or(a: Array[Boolean], b: Array[Boolean]): Array[Boolean] = a || b
end BooleanArrayOps
