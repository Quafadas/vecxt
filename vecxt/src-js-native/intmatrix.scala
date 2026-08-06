package vecxt

import vecxt.dimensionExtender.DimensionExtender.*
import vecxt.matrix.Matrix
import scala.annotation.targetName

object JvmIntMatrix:
  extension (m: Matrix[Int])

    /** `hasSimpleContiguousMemoryLayout` accepts dense row-major as well as dense column-major (see its own scaladoc),
      * so the index into `m.raw` has to go through `m.layout.linearIndex` rather than the col-major-only
      * `col * m.rows + row` — otherwise a dense row-major `m` passes the guard and is then read as if it were
      * column-major, silently attributing each row/col's accumulated value to the wrong row/col. `linearIndex` is
      * `@Thin`, so this costs nothing over the hardcoded formula it replaces.
      */
    private inline def reduceAlongDimension(
        dim: DimensionExtender,
        inline op: (Int, Int) => Int,
        inline initial: Int
    ): Matrix[Int] =
      if !m.hasSimpleContiguousMemoryLayout then ???
      end if
      val whichDim = dim.asInt
      val newShape = m.shape match
        case (r, c) if whichDim == 0 => (r, 1)
        case (r, c) if whichDim == 1 => (1, c)
        case _                       => ???

      val newArr = Array.fill(newShape._1 * newShape._2)(initial)
      var i = 0
      while i < m.cols do
        var j = 0
        while j < m.rows do
          val idx = m.layout.linearIndex(j, i)
          if whichDim == 0 then newArr(j) = op(newArr(j), m.raw(idx))
          end if
          if whichDim == 1 then newArr(i) = op(newArr(i), m.raw(idx))
          end if
          j += 1
        end while
        i += 1
      end while

      Matrix[Int](newArr, newShape)
    end reduceAlongDimension

    @targetName("intMatrixMax")
    def max(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, math.max, Int.MinValue)
    end max

    @targetName("intMatrixMin")
    def min(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, math.min, Int.MaxValue)
    end min

    @targetName("intMatrixSum")
    def sum(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, _ + _, 0)
    end sum

    @targetName("intMatrixProduct")
    def product(dim: DimensionExtender): Matrix[Int] =
      reduceAlongDimension(dim, _ * _, 1)
    end product

  end extension
end JvmIntMatrix
