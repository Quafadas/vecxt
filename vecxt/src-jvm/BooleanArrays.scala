package vecxt

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.VectorMask

object BooleanArrays:

  private final val spb = ByteVector.SPECIES_PREFERRED
  private final val spbl = spb.length()

  extension (vec: Array[Boolean])
    // TODO, benchmark
    def allTrue: Boolean =
      var out = true
      var i = 0
      breakable {
        val bound = spb.loopBound(vec.length)
        while i < bound do
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

    def any: Boolean =
      var out = false
      var i = 0
      breakable {
        val bound = spb.loopBound(vec.length)
        while i < bound do
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

    def trues: Int =
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

    def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      val bound = spb.loopBound(vec.length)
      while i < bound do
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

    def not: Array[Boolean] =
      val dup = vec.clone()
      dup.`not!`
      dup
    end not

    def `not!`: Unit =
      var i = 0

      val bound = spb.loopBound(vec.length)
      while i < bound do
        ByteVector
          .fromBooleanArray(spb, vec, i)
          .not()
          .intoBooleanArray(vec, i)
        i += spbl
      end while

      while i < vec.length do
        vec(i) = !vec(i)
        i += 1
      end while
    end `not!`

    def ||(thatIdx: Array[Boolean]): Array[Boolean] =

      val result: Array[Boolean] = new Array[Boolean](vec.length)
      var i = 0

      val bound = spb.loopBound(vec.length)
      while i < bound do
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
