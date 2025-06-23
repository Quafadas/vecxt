package vecxt.experiments

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout

opaque type DoubleVector = MemorySegment

export DoubleVector.*

object DoubleVector {

  extension (v: DoubleVector) {

    inline def apply(index: Long): Double =
      v.getAtIndex(ValueLayout.JAVA_DOUBLE, index)

    inline def update(index: Long, value: Double): Unit =
      v.setAtIndex(ValueLayout.JAVA_DOUBLE, index, value)

    inline def length: Long =
      v.byteSize() / ValueLayout.JAVA_DOUBLE.byteSize()
  }

}

extension (dv: DoubleVector.type)
  inline def apply(size: Long)(using arena: Arena): DoubleVector =
    arena.allocate(ValueLayout.JAVA_DOUBLE, size)

  inline def apply(data: Seq[Double])(using arena: Arena): DoubleVector =
    arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE,
      data*
    )
