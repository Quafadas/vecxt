package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.arrays.*
import vecxt.matrix.*

object JvmIntMatrix:
  extension (m: Matrix[Int])

    inline def matmul(b: Matrix[Int])(using inline boundsCheck: BoundsCheck): Matrix[Int] =
      dimMatCheck(m, b)
      ???

    end matmul

    inline def >=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end >=

    inline def >(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.gt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end >

    inline def <=(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lte(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end <=

    inline def <(d: Int): Matrix[Boolean] =
      if m.hasSimpleContiguousMemoryLayout then
        val i: Array[Int] = m.raw
        Matrix[Boolean](m.raw.lt(d), m.shape)(using BoundsCheck.DoBoundsCheck.no)
      else ???
    end <

  end extension
end JvmIntMatrix

object NativeIntMatrix:

end NativeIntMatrix
