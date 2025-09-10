package vecxt.experiments

import java.lang.foreign.{Arena, MemorySegment, ValueLayout, SegmentAllocator}
import blis.*
import _root_.vecxt.all.*
import narr.*
import _root_.vecxt.BoundsCheck.DoBoundsCheck.yes
import mlx.{mlx_h, mlx_string, mlx_metal_device_info_t_}
import MlxString.*
import MlxArray.*
import MlxStream.*

/**
  * /* Copyright © 2023-2024 Apple Inc. */

#include <stdio.h>
#include "mlx/c/mlx.h"

void print_array(const char* msg, mlx_array arr) {
  mlx_string str = mlx_string_new();
  mlx_array_tostring(&str, arr);
  printf("%s\n%s\n", msg, mlx_string_data(str));
  mlx_string_free(str);
}

void gpu_info() {
  printf("==================================================\n");
  printf("GPU info:\n");
  mlx_metal_device_info_t info = mlx_metal_device_info();
  printf("architecture: %s\n", info.architecture);
  printf("max_buffer_length: %ld\n", info.max_buffer_length);
  printf(
      "max_recommended_working_set_size: %ld\n",
      info.max_recommended_working_set_size);
  printf("memory_size: %ld\n", info.memory_size);

  printf("==================================================\n");
}
int main() {
  mlx_string version = mlx_string_new();
  mlx_version(&version);
  printf("MLX version: %s\n", mlx_string_data(version));

  gpu_info();

  mlx_stream stream = mlx_default_gpu_stream_new();
  float data[] = {1, 2, 3, 4, 5, 6};
  int shape[] = {2, 3};
  mlx_array arr = mlx_array_new_data(data, shape, 2, MLX_FLOAT32);
  print_array("hello world!", arr);

  mlx_array two = mlx_array_new_int(2);
  mlx_divide(&arr, arr, two, stream);
  print_array("divive by 2!", arr);

  mlx_arange(&arr, 0, 3, 0.5, MLX_FLOAT32, stream);
  print_array("arange", arr);

  mlx_array_free(arr);
  mlx_array_free(two);
  mlx_stream_free(stream);
  mlx_string_free(version);
  return 0;
}

  *
  */



@main def mlxDemo(): Unit = {
  println("MLX Demo - Scala version of the C code")
  val arena = Arena.ofConfined()

  try {
    given thisArena: Arena = arena

    val deviceInfo = MetalDeviceInfo.getDeviceInfo(using arena)
    println("\n=== Testing MetalDeviceInfo wrapper ===")
    println(s"Info: ${deviceInfo}")

    // Test creating a string with data
    println("\n=== String creation with data test ===")
    val myString: MlxString = MlxString.createString("Hello, MLX!")(using arena)
    println(s"Created string data: ${myString.show}")

    // Test creating and printing an MLX array
    println("\n=== Array creation test ===")

    val shape = Array(2, 3)  // 2x3 matrix

    val floatData: MlxArray = MlxArray.floatArray(
      Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Some(shape)
    )
    val floatData2: MlxArray = MlxArray.floatArray(
      Array(5.0f, 2.0f, 7.0f, 45.0f, 5.5f, 6.0f), Some(shape)
    )

    val gpu: MlxStream = MlxStream.gpu
    val cpu: MlxStream = MlxStream.cpu
    println(s"Created MLX stream: ${cpu.show}")
    println(s"Created MLX stream: ${gpu.show}")

    // adding arrays
    val addedG: MlxArray = MlxArray.add(floatData, floatData2, gpu) // Just to test addition
    val addedC: MlxArray = MlxArray.add(floatData, floatData2, cpu) // Just to test addition

    println(s"Created MLX arrays:")
    println(floatData.show)
    println(floatData2.show)
    println(s"Added on CPU:")
    println(addedC.show)
    println(s"Added on GPU:")
    println(addedG.show)
  }

  finally {
    arena.close()
  }
}
