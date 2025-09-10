package vecxt.experiments

import java.lang.foreign.{Arena, MemorySegment}
import mlx.{mlx_h_1, mlx_string_}

object MlxStream {

  private lazy val defaultGpuStreamInvoker = mlx_h_1.mlx_default_gpu_stream_new.makeInvoker()
  private lazy val defaultCpuStreamInvoker = mlx_h_1.mlx_default_cpu_stream_new.makeInvoker()
  private lazy val stringInvoker = mlx_h_1.mlx_string_new.makeInvoker()

  opaque type MlxStream = MemorySegment

  extension (stream: MlxStream)
    inline def raw: MemorySegment = stream
    inline def show(using arena: Arena): String = {
      val mlxString = arena.allocate(mlx_string_.layout())
      val status = mlx_h_1.mlx_stream_tostring(mlxString, stream)
      if status == 0 then
        // Use the proper MLX string data extraction method
        val dataPtr = mlx_h_1.mlx_string_data(mlxString)
        if dataPtr.address() == 0 then
          "<empty stream>"
        else
          dataPtr.getString(0)
      else
        s"<MlxStream: conversion failed with status $status>"
    }

  inline def gpu(using arena: Arena): MlxStream =
    defaultGpuStreamInvoker.apply(arena)

  inline def cpu(using arena: Arena): MlxStream =
    defaultCpuStreamInvoker.apply(arena)


}
