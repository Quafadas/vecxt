package vecxt.experiments

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout


opaque type IntVector = MemorySegment

export IntVector.*

object IntVector {

  extension (v: IntVector) {

    inline def raw: MemorySegment = v

    inline def apply(index: Long): Int =
      v.getAtIndex(ValueLayout.JAVA_INT, index)

    inline def update(index: Long, value: Int): Unit =
      v.setAtIndex(ValueLayout.JAVA_INT, index, value)

    inline def length: Long =
      v.byteSize() / ValueLayout.JAVA_INT.byteSize()

    inline def copy(using arena: Arena): IntVector =
      val newM = arena.allocate(ValueLayout.JAVA_INT, v.length)
      MemorySegment.copy(v, 0L, newM, 0L, v.byteSize())
      newM

    inline def toSeq: Seq[Int] =
      (0L until v.length).map(i => v.getAtIndex(ValueLayout.JAVA_INT, i))

    /**
      * Element-wise addition using simple loops (cross-platform compatible)
      */
    def +=(vec2: IntVector): Unit = ???

    def +(vec2: IntVector)(using arena: Arena): IntVector = {
      val result = v.copy
      result += vec2
      result
    }

}

extension (iv: IntVector.type)

  inline def ofSize(size: Long)(using arena: Arena): IntVector =
    arena.allocate(ValueLayout.JAVA_INT, size)

  inline def apply(size: Long)(using arena: Arena): IntVector =
    arena.allocate(ValueLayout.JAVA_INT, size)

  inline def apply(data: Seq[Int])(using arena: Arena): IntVector =
    arena.allocateFrom(
      ValueLayout.JAVA_INT,
      data*
    )
