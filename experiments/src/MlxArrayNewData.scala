package vecxt.experiments

import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import mlx.{mlx_h, mlx_array, mlx_string_}
import MlxString.{getStringData, asString}

object MlxArrayNewData:

  opaque type MlxArray = MemorySegment

  extension (arr: MlxArray)
    def show(using arena: Arena): String =
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

  def newArrayRaw(
    data: MemorySegment,
    shape: MemorySegment,
    dim: Int,
    dtype: Int
  )(using arena: Arena): MlxArray =
    mlx.mlx_h_1.mlx_array_new_data(arena, data, shape, dim, dtype)

  def newArray(
    data: MemorySegment,
    shape: MemorySegment,
    dim: Int,
    dtype: Int
  )(using arena: Arena): MlxArray =
    mlx.mlx_h_1.mlx_array_new_data(arena, data, shape, dim, dtype)

end MlxArrayNewData
