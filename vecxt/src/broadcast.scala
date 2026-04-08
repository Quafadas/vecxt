package vecxt

import vecxt.ndarray.*

object broadcast:

  class BroadcastException(msg: String) extends RuntimeException(msg)
  class ShapeMismatchException(msg: String) extends RuntimeException(msg)

  /** True if two shapes have the same rank and element-wise equal dimensions. */
  inline def sameShape(a: Array[Int], b: Array[Int]): Boolean =
    if a.length != b.length then false
    else
      var i = 0
      var eq = true
      while i < a.length && eq do
        if a(i) != b(i) then eq = false
        end if
        i += 1
      end while
      eq

  /** Compute the output shape for broadcasting two shapes.
    *
    * Shapes are right-aligned; a dimension of 1 expands to match the other. Throws `BroadcastException` if shapes are
    * incompatible.
    */
  inline def broadcastShape(a: Array[Int], b: Array[Int]): Array[Int] =
    val n = math.max(a.length, b.length)
    val out = new Array[Int](n)
    var i = 0
    while i < n do
      val ai = i - (n - a.length)
      val bi = i - (n - b.length)
      val da = if ai < 0 then 1 else a(ai)
      val db = if bi < 0 then 1 else b(bi)
      if da == db then out(i) = da
      else if da == 1 then out(i) = db
      else if db == 1 then out(i) = da
      else
        throw BroadcastException(
          s"Cannot broadcast shapes [${a.mkString(",")}] and [${b.mkString(",")}]: " +
            s"incompatible at dimension $i ($da vs $db)"
        )
      end if
      i += 1
    end while
    out
  end broadcastShape

  /** Compute broadcast-extended strides for `arr` viewed as having `outShape`.
    *
    * Prepends 0s for dimensions padded on the left; sets stride to 0 for original dimensions of size 1 (broadcast).
    */
  inline def broadcastStrides(arr: NDArray[?], outShape: Array[Int]): Array[Int] =
    val n = outShape.length
    val strides = new Array[Int](n)
    var i = 0
    while i < n do
      val arrIdx = i - (n - arr.ndim)
      if arrIdx < 0 then strides(i) = 0
      else if arr.shape(arrIdx) == 1 then strides(i) = 0
      else strides(i) = arr.strides(arrIdx)
      end if
      i += 1
    end while
    strides
  end broadcastStrides

  extension [A](arr: NDArray[A])

    /** Return a zero-copy view of this NDArray broadcast to `targetShape`.
      *
      * Dimensions of size 1 are expanded via stride 0; prepended dimensions (when `targetShape` has more dims) also get
      * stride 0. Throws `BroadcastException` if shapes are incompatible.
      */
    inline def broadcastTo(targetShape: Array[Int]): NDArray[A] =
      if sameShape(arr.shape, targetShape) then arr
      else
        if arr.ndim > targetShape.length then
          throw BroadcastException(
            s"Cannot broadcast shape [${arr.shape.mkString(",")}] to [${targetShape.mkString(",")}]: " +
              s"source has more dimensions than target"
          )
        end if
        val n = targetShape.length
        var i = 0
        while i < n do
          val arrIdx = i - (n - arr.ndim)
          val da = if arrIdx < 0 then 1 else arr.shape(arrIdx)
          val dt = targetShape(i)
          if da != dt && da != 1 then
            throw BroadcastException(
              s"Cannot broadcast shape [${arr.shape.mkString(",")}] to [${targetShape.mkString(",")}]: " +
                s"incompatible at dimension $i ($da vs $dt)"
            )
          end if
          i += 1
        end while
        mkNDArray(arr.data, targetShape.clone(), broadcastStrides(arr, targetShape), arr.offset)
  end extension

  /** Broadcast both operands to their common shape.
    *
    * Returns `(a', b')` where both have `shape == broadcastShape(a.shape, b.shape)`. Throws `BroadcastException` if
    * shapes are incompatible.
    */
  inline def broadcastPair[A](a: NDArray[A], b: NDArray[A]): (NDArray[A], NDArray[A]) =
    val outShape = broadcastShape(a.shape, b.shape)
    (a.broadcastTo(outShape), b.broadcastTo(outShape))
  end broadcastPair

end broadcast
