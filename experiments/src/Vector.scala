package vecxt.experiments

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout
import blis_typed.blis_h


opaque type DoubleVector = MemorySegment

export DoubleVector.*

object DoubleVector {

  extension (v: DoubleVector) {

    inline def raw: MemorySegment = v

    inline def apply(index: Long): Double =
      v.getAtIndex(ValueLayout.JAVA_DOUBLE, index)

    inline def update(index: Long, value: Double): Unit =
      v.setAtIndex(ValueLayout.JAVA_DOUBLE, index, value)

    inline def length: Long =
      v.byteSize() / ValueLayout.JAVA_DOUBLE.byteSize()

    inline def copy(using arena: Arena): DoubleVector =
      val newM = arena.allocate(ValueLayout.JAVA_DOUBLE, v.length)
      MemorySegment.copy(v,0L, newM, 0L, v.byteSize())
      newM

    inline def toSeq: Seq[Double] =
      (0L until v.length).map(i => v.getAtIndex(ValueLayout.JAVA_DOUBLE, i))

    /**
      * This will allocate an object that will be freed after the arena is closed. It does _not_ cleanup this memory segment itself on free.
      *
      * https://github.com/flame/blis/blob/master/docs/BLISObjectAPI.md#object-management
      * > Objects initialized via this function should generally not be passed to bli_obj_free(), unless the user wishes to pass p into free().
      * @param arena
      * @return
      */
    inline def blis_obj_t(using arena: Arena) = {
      val objSegment = arena.allocate(512L)

      blis_h.bli_obj_create_with_attached_buffer(
        blis_h.BLIS_DOUBLE(),  // dt: BLIS_DOUBLE for double precision
        1L,                  // m: 1 row (row vector)
        v.length,            // n: length columns
        v.raw,               // p: pointer to the actual data
        v.length,            // rs: row stride = length (distance between rows)
        1L,                  // cs: column stride = 1 (contiguous elements)
        objSegment           // obj: output object
      )
      objSegment
    }

    def +=(vec2: DoubleVector)(using arena: Arena): Unit =
      blis_h.bli_addv(vec2.blis_obj_t, v.blis_obj_t)

    def +(vec2: DoubleVector)(using arena: Arena): DoubleVector = {
      val result = v.copy
      result += vec2
      result
    }
  }

}

extension (dv: DoubleVector.type)

  inline def ofSize(size: Long)(using arena: Arena): DoubleVector =
    arena.allocate(ValueLayout.JAVA_DOUBLE, size)

  inline def apply(size: Long)(using arena: Arena): DoubleVector =
    arena.allocate(ValueLayout.JAVA_DOUBLE, size)

  inline def apply(data: Seq[Double])(using arena: Arena): DoubleVector =
    arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE,
      data*
    )
