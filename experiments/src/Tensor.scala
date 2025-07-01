package vecxt.experiments

type Tensor[A] = A match
  case Double => TensorDouble
  case Int    => TensorInt
  case Float  => TensorFloat

object TensorOps:
  private[experiments] def calculateFlatIndex(indices: IArray[Long], strides: IArray[Long], offset: Long): Long =
    var flatIndex = offset
    var i = 0
    while i < indices.length do
      flatIndex += indices(i) * strides(i)
      i += 1
    end while
    flatIndex
  end calculateFlatIndex
end TensorOps

class TensorDouble(val data: DoubleVector, val shape: IArray[Long], val strides: IArray[Long], val offset: Long = 0L)
    extends TensorOps[Double]:

  def apply(tensor: Tensor[Double], indices: IArray[Long]): Double =
    val td = tensor.asInstanceOf[TensorDouble]
    val flatIndex = TensorOps.calculateFlatIndex(indices, td.strides, td.offset)
    td.data(flatIndex)
  end apply

  def update(tensor: Tensor[Double], indices: IArray[Long], value: Double): Unit =
    val td = tensor.asInstanceOf[TensorDouble]
    val flatIndex = TensorOps.calculateFlatIndex(indices, td.strides, td.offset)
    td.data(flatIndex) = value
  end update

  def shape(tensor: Tensor[Double]): IArray[Long] =
    tensor.asInstanceOf[TensorDouble].shape

  def strides(tensor: Tensor[Double]): IArray[Long] =
    tensor.asInstanceOf[TensorDouble].strides

  def offset(tensor: Tensor[Double]): Long =
    tensor.asInstanceOf[TensorDouble].offset
end TensorDouble

class TensorInt(val data: IntVector, val shape: IArray[Long], val strides: IArray[Long], val offset: Long = 0L)
    extends TensorOps[Int]:

  def apply(tensor: Tensor[Int], indices: IArray[Long]): Int =
    val ti = tensor.asInstanceOf[TensorInt]
    val flatIndex = TensorOps.calculateFlatIndex(indices, ti.strides, ti.offset)
    ti.data(flatIndex)
  end apply

  def update(tensor: Tensor[Int], indices: IArray[Long], value: Int): Unit =
    val ti = tensor.asInstanceOf[TensorInt]
    val flatIndex = TensorOps.calculateFlatIndex(indices, ti.strides, ti.offset)
    ti.data(flatIndex) = value
  end update

  def shape(tensor: Tensor[Int]): IArray[Long] =
    tensor.asInstanceOf[TensorInt].shape

  def strides(tensor: Tensor[Int]): IArray[Long] =
    tensor.asInstanceOf[TensorInt].strides

  def offset(tensor: Tensor[Int]): Long =
    tensor.asInstanceOf[TensorInt].offset
end TensorInt

class TensorFloat(val data: FloatVector, val shape: IArray[Long], val strides: IArray[Long], val offset: Long = 0L)
    extends TensorOps[Float]:

  def apply(tensor: Tensor[Float], indices: IArray[Long]): Float =
    val tf = tensor.asInstanceOf[TensorFloat]
    val flatIndex = TensorOps.calculateFlatIndex(indices, tf.strides, tf.offset)
    tf.data(flatIndex)
  end apply

  def update(tensor: Tensor[Float], indices: IArray[Long], value: Float): Unit =
    val tf = tensor.asInstanceOf[TensorFloat]
    val flatIndex = TensorOps.calculateFlatIndex(indices, tf.strides, tf.offset)
    tf.data(flatIndex) = value
  end update

  def shape(tensor: Tensor[Float]): IArray[Long] =
    tensor.asInstanceOf[TensorFloat].shape

  def strides(tensor: Tensor[Float]): IArray[Long] =
    tensor.asInstanceOf[TensorFloat].strides

  def offset(tensor: Tensor[Float]): Long =
    tensor.asInstanceOf[TensorFloat].offset
end TensorFloat

trait TensorOps[A]:
  def apply(tensor: Tensor[A], indices: IArray[Long]): A
  def update(tensor: Tensor[A], indices: IArray[Long], value: A): Unit
  def shape(tensor: Tensor[A]): IArray[Long]
  def strides(tensor: Tensor[A]): IArray[Long]
  def offset(tensor: Tensor[A]): Long
end TensorOps
