package vecxt.experiments

import java.lang.foreign.{Arena, MemorySegment}
import mlx.{mlx_h, mlx_string, mlx_string_}

object MlxString:

  extension (mlxString: MemorySegment)
    def getStringData: String = asString(mlxString)

  // Create an mlx_string structure by allocating memory for it and populate with string data
  def createString(s: String)(using arena: Arena): MemorySegment =
    val mlxString = arena.allocate(mlx_string_.layout())
    // Allocate memory for the string data and copy it
    val stringData = arena.allocateFrom(s)
    // Set the ctx pointer to point to our string data
    mlx_string_.ctx(mlxString, stringData)
    mlxString

  def createStringRaw(arena: Arena): MemorySegment =
    arena.allocate(mlx_string_.layout())

  // Extract string data from an mlx_string after it's been populated
  def asString(mlxString: MemorySegment): String =
    val ctx = mlx_string_.ctx(mlxString)
    if ctx.address() == 0 then
      ""  // null pointer
    else
      ctx.getString(0)  // Read null-terminated string from the context pointer


  // Helper to print any mlx_string to console
  def printString(mlxString: MemorySegment, label: String = "String"): Unit =
    val data = getStringData(mlxString)
    println(s"$label: $data")

end MlxString
