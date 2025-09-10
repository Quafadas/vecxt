package vecxt.experiments

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout

import blis_typed.blis_h

opaque type FloatVector = MemorySegment

object FloatVector:

  extension (v: FloatVector)

    inline def raw: MemorySegment = v

    inline def apply(index: Long): Float =
      v.getAtIndex(ValueLayout.JAVA_FLOAT, index)

    inline def update(index: Long, value: Float): Unit =
      v.setAtIndex(ValueLayout.JAVA_FLOAT, index, value)

    inline def length: Long =
      v.byteSize() / ValueLayout.JAVA_FLOAT.byteSize()

    inline def copy(using arena: Arena): FloatVector =
      val newM = arena.allocate(ValueLayout.JAVA_FLOAT, v.length)
      MemorySegment.copy(v, 0L, newM, 0L, v.byteSize())
      newM
    end copy

    inline def toSeq: Seq[Float] =
      (0L until v.length).map(i => v.getAtIndex(ValueLayout.JAVA_FLOAT, i))

    /** This will allocate an object that will be freed after the arena is closed. It does _not_ cleanup this memory
      * segment itself on free.
      *
      * https://github.com/flame/blis/blob/master/docs/BLISObjectAPI.md#object-management > Objects initialized via this
      * function should generally not be passed to bli_obj_free(), unless the user wishes to pass p Floato free().
      * @param arena
      * @return
      */
    inline def blis_obj_t(using arena: Arena) =
      val objSegment = arena.allocate(512L)

      blis_h.bli_obj_create_with_attached_buffer(
        blis_h.BLIS_FLOAT(), // dt: BLIS_DOUBLE for double precision
        1L, // m: 1 row (row vector)
        v.length, // n: length columns
        v.raw, // p: poFloater to the actual data
        v.length, // rs: row stride = length (distance between rows)
        1L, // cs: column stride = 1 (contiguous elements)
        objSegment // obj: output object
      )
      objSegment
    end blis_obj_t

    def +=(vec2: FloatVector)(using Arena): Unit =
      blis_h.bli_addv(vec2.blis_obj_t, v.blis_obj_t)

    def +(vec2: FloatVector)(using arena: Arena): FloatVector =
      val result = v.copy
      result += vec2
      result
    end +
  end extension

  // Static methods for FloatVector creation

  inline def ofSize(size: Long)(using arena: Arena): FloatVector =
    arena.allocate(ValueLayout.JAVA_FLOAT, size)

  inline def apply(size: Long)(using arena: Arena): FloatVector =
    arena.allocate(ValueLayout.JAVA_FLOAT, size)

  inline def apply(data: Seq[Float])(using arena: Arena): FloatVector =
    arena.allocateFrom(
      ValueLayout.JAVA_FLOAT,
      data*
    )
end FloatVector
