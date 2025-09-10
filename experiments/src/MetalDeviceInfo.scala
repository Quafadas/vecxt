package vecxt.experiments

import java.lang.foreign.{Arena, MemorySegment, SegmentAllocator}
import mlx.{mlx_h, mlx_metal_device_info_t_}

object MetalDeviceInfo {

  private lazy val deviceInfoInvoker = mlx_h.mlx_metal_device_info.makeInvoker()

  private def getDeviceInfoRaw(arena: Arena): MemorySegment = {
    deviceInfoInvoker.apply(arena)
  }

  def getDeviceInfo(using arena: Arena): (
    architecture: String,
    maxBufferLength: Long,
    maxRecommendedWorkingSetSize: Long,
    memorySize: Long
  ) = {
    val segment = deviceInfoInvoker.apply(arena)
    (
      architecture = mlx_metal_device_info_t_.architecture(segment).getString(0),
      maxBufferLength = mlx_metal_device_info_t_.max_buffer_length(segment),
      maxRecommendedWorkingSetSize = mlx_metal_device_info_t_.max_recommended_working_set_size(segment),
      memorySize = mlx_metal_device_info_t_.memory_size(segment)
    )
  }

}
