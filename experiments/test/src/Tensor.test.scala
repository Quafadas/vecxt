package vecxt.experiments

import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout

class TensorSuite extends munit.FunSuite:

  given arena: BlisArena = BlisArena(Arena.global())

  test("TensorDouble basic operations"):
    val data = DoubleVector(6)
    // Fill with test data: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    data(0) = 1.0
    data(1) = 2.0
    data(2) = 3.0
    data(3) = 4.0
    data(4) = 5.0
    data(5) = 6.0

    // Create a 2x3 tensor with column-major layout
    val shape = IArray(2L, 3L)
    val strides = IArray(1L, 2L) // column-major: stride 1 for rows, stride 2 for columns
    val tensor = TensorDouble(data, shape, strides)

    // Test shape, strides, offset
    assertEquals(tensor.shape(tensor), shape)
    assertEquals(tensor.strides(tensor), strides)
    assertEquals(tensor.offset(tensor), 0L)

    // Test indexing: tensor(0, 0) should be data(0) = 1.0
    assertEquals(tensor(tensor, IArray(0L, 0L)), 1.0)
    // Test indexing: tensor(1, 0) should be data(1) = 2.0
    assertEquals(tensor(tensor, IArray(1L, 0L)), 2.0)
    // Test indexing: tensor(0, 1) should be data(2) = 3.0
    assertEquals(tensor(tensor, IArray(0L, 1L)), 3.0)
    // Test indexing: tensor(1, 2) should be data(5) = 6.0
    assertEquals(tensor(tensor, IArray(1L, 2L)), 6.0)

    // Test update
    tensor.update(tensor, IArray(0L, 0L), 10.0)
    assertEquals(tensor(tensor, IArray(0L, 0L)), 10.0)
    assertEquals(data(0), 10.0) // Verify the underlying data was modified

  test("TensorInt basic operations"):
    val data = IntVector(4)
    data(0) = 10
    data(1) = 20
    data(2) = 30
    data(3) = 40

    // Create a 2x2 tensor
    val shape = IArray(2L, 2L)
    val strides = IArray(1L, 2L)
    val tensor = TensorInt(data, shape, strides)

    assertEquals(tensor.shape(tensor), shape)
    assertEquals(tensor.strides(tensor), strides)
    assertEquals(tensor.offset(tensor), 0L)

    assertEquals(tensor(tensor, IArray(0L, 0L)), 10)
    assertEquals(tensor(tensor, IArray(1L, 1L)), 40)

    tensor.update(tensor, IArray(1L, 0L), 99)
    assertEquals(tensor(tensor, IArray(1L, 0L)), 99)

  test("TensorFloat basic operations"):
    val data = FloatVector(4)
    data(0) = 1.5f
    data(1) = 2.5f
    data(2) = 3.5f
    data(3) = 4.5f

    // Create a 2x2 tensor
    val shape = IArray(2L, 2L)
    val strides = IArray(1L, 2L)
    val tensor = TensorFloat(data, shape, strides)

    assertEquals(tensor.shape(tensor), shape)
    assertEquals(tensor.strides(tensor), strides)
    assertEquals(tensor.offset(tensor), 0L)

    assertEquals(tensor(tensor, IArray(0L, 0L)), 1.5f)
    assertEquals(tensor(tensor, IArray(1L, 1L)), 4.5f)

    tensor.update(tensor, IArray(0L, 1L), 9.9f)
    assertEquals(tensor(tensor, IArray(0L, 1L)), 9.9f)

  test("TensorDouble with offset"):
    val data = DoubleVector(10)
    (0 until 10).foreach(i => data(i) = i.toDouble)

    // Create a tensor starting from offset 3
    val shape = IArray(2L, 2L)
    val strides = IArray(1L, 2L)
    val offset = 3L
    val tensor = TensorDouble(data, shape, strides, offset)

    assertEquals(tensor.offset(tensor), offset)

    // tensor(0, 0) should access data(3) = 3.0
    assertEquals(tensor(tensor, IArray(0L, 0L)), 3.0)
    // tensor(1, 1) should access data(3 + 1*1 + 1*2) = data(6) = 6.0
    assertEquals(tensor(tensor, IArray(1L, 1L)), 6.0)

end TensorSuite
