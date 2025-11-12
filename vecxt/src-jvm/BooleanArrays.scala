package vecxt

import scala.util.control.Breaks.*
import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.VectorMask

object BooleanArrays:

  private final val spb = ByteVector.SPECIES_PREFERRED
  private final val spbl = spb.length()

  extension (vec: Array[Boolean])
    // TODO, benchmark
    inline def allTrue: Boolean =
      var out = true
      var i = 0
      breakable {
        while i < spb.loopBound(vec.length) do
          if !VectorMask.fromArray(spb, vec, i).allTrue then
            out = false
            break
          end if
          i += spbl
        end while
      }

      if out then
        while i < vec.length do
          if !vec(i) then out = false
          end if
          i += 1
        end while

      end if
      out
    end allTrue

    inline def any: Boolean =
      var out = false
      var i = 0
      breakable {
        while i < spb.loopBound(vec.length) do
          if VectorMask.fromArray(spb, vec, i).anyTrue() then
            out = true
            break
          end if
          i += spbl
        end while
      }

      if !out then
        while i < vec.length do
          if vec(i) then out = true
          end if
          i += 1
        end while

      end if
      out
    end any

    inline def trues: Int =
      var i = 0
      var sum = 0

      while i < spb.loopBound(vec.length) do
        sum += VectorMask.fromArray(spb, vec, i).trueCount()
        i += spbl
      end while

      while i < vec.length do
        if vec(i) then sum += 1
        end if
        i += 1
      end while
      sum
    end trues

    inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < spb.loopBound(vec.length) do
        ByteVector
          .fromBooleanArray(spb, vec, i)
          .and(ByteVector.fromBooleanArray(spb, thatIdx, i))
          .intoBooleanArray(result, i)
        i += spbl
      end while

      while i < vec.length do
        result(i) = vec(i) && thatIdx(i)
        i += 1
      end while
      result
    end &&

    inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =

      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      while i < spb.loopBound(vec.length) do
        ByteVector
          .fromBooleanArray(spb, vec, i)
          .or(ByteVector.fromBooleanArray(spb, thatIdx, i))
          .intoBooleanArray(result, i)
        i += spbl
      end while

      while i < vec.length do
        result(i) = vec(i) || thatIdx(i)
        i += 1
      end while
      result
    end ||
  end extension
end BooleanArrays
