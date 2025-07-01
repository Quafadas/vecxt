package vecxt.experiments

import java.lang.foreign.MemorySegment
import java.lang.foreign.MemoryLayout
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout
import blis_typed.blis_h
import scala.collection.mutable

/** This _should_ be unecessary for blis objects initialised with `blis_obj_create_with_attached_buffer`. From the docs
  * of BLIS blis_obj_create_with_attached_buffer:
  *
  * > Objects initialized via this function should generally not be passed to bli_obj_free(), unless the user wishes to
  * pass p into free().
  *
  * @param underlying
  */
class BlisArena(private val underlying: Arena) extends Arena:
  private val blisObjects = mutable.ListBuffer[MemorySegment]()

  inline def allocate(byteSize: Long, byteAlignment: Long): MemorySegment = underlying.allocate(byteSize, byteAlignment)

  inline def scope = underlying.scope

  def registerBlisObject(obj: MemorySegment): Unit =
    blisObjects += obj

  override def close(): Unit =
    // Free all BLIS objects first
    blisObjects.foreach(blis_h.bli_obj_free)
    blisObjects.clear()

    // Then close the underlying arena
    underlying.close()
  end close
end BlisArena
