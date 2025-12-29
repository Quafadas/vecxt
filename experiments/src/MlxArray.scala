package vecxt.experiments

import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import mlx.{mlx_h, mlx_array_, mlx_string_}
import MlxString.{show, asString}
import MlxStream.MlxStream

object MlxArray:

  private lazy val asFloat32 = mlx.mlx_h_1.MLX_FLOAT32()

  opaque type MlxArray = MemorySegment

  extension (arr: MlxArray)
    inline def show(using arena: Arena): String =
      val mlxString = arena.allocate(mlx_string_.layout())
      val status = mlx.mlx_h_1.mlx_array_tostring(mlxString, arr)
      if status == 0 then
        // Use the proper MLX string data extraction method
        val dataPtr = mlx.mlx_h_1.mlx_string_data(mlxString)
        if dataPtr.address() == 0 then
          "<empty>"
        else
          dataPtr.getString(0)
      else
        s"<MlxArray: conversion failed with status $status>"



  inline def add(a: MlxArray, b: MlxArray, stream: MlxStream)(using arena: Arena): MlxArray =
    val result = arena.allocate(mlx_array_.layout())
    val status = mlx_h.mlx_add(result, a, b, stream.raw)
    if status != 0 then
      throw new RuntimeException(s"mlx_add failed with status $status")
    result

  inline def floatArray(data: Array[Float], shapeSegment: Option[Array[Int]] = None)(using arena: Arena): MlxArray =
    val dataSegment = arena.allocateFrom(ValueLayout.JAVA_FLOAT, data*)
    val shapeSegment_ = arena.allocateFrom(ValueLayout.JAVA_INT, shapeSegment.getOrElse(Array(1))*)
    newArray(dataSegment, shapeSegment_, shapeSegment.fold(1)(_.length), asFloat32)

  inline def newArrayRaw(
    data: MemorySegment,
    shape: MemorySegment,
    dim: Int,
    dtype: Int
  )(using arena: Arena): MlxArray =
    mlx.mlx_h_1.mlx_array_new_data(arena, data, shape, dim, dtype)

  inline def newArray(
    data: MemorySegment,
    shape: MemorySegment,
    dim: Int,
    dtype: Int
  )(using arena: Arena): MlxArray =
    mlx.mlx_h_1.mlx_array_new_data(arena, data, shape, dim, dtype)

end MlxArray
