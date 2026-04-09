package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.broadcast.*
import vecxt.ndarray.*

object NDArrayBooleanIndexing:

  extension [A: ClassTag](arr: NDArray[A])

    /** Select elements where mask is true. Returns a 1-D col-major NDArray. Mask must have the same shape as arr.
      * Result length = mask.countTrue.
      */
    inline def apply(mask: NDArray[Boolean])(using inline bc: BoundsCheck): NDArray[A] =
      inline if bc then
        if !sameShape(arr.shape, mask.shape) then
          throw ShapeMismatchException(
            s"Boolean indexing requires arr and mask to have the same shape: [${arr.shape.mkString(",")}] vs [${mask.shape.mkString(",")}]."
          )
        end if
      end if
      if arr.isColMajor && mask.isColMajor then
        // Fast path: flat array iteration
        var count = 0
        var i = 0
        while i < mask.data.length do
          if mask.data(i) then count += 1
          end if
          i += 1
        end while
        val out = new Array[A](count)
        var j = 0
        i = 0
        while i < arr.data.length do
          if mask.data(i) then
            out(j) = arr.data(i)
            j += 1
          end if
          i += 1
        end while
        mkNDArray(out, Array(count), Array(1), 0)
      else
        // General path: stride iteration
        val n = arr.numel
        val ndim = arr.ndim
        val cumProd = colMajorStrides(arr.shape)

        // First pass: count trues
        var count = 0
        var j = 0
        while j < n do
          var posMask = mask.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % arr.shape(k)
            posMask += coord * mask.strides(k)
            k += 1
          end while
          if mask.data(posMask) then count += 1
          end if
          j += 1
        end while

        val out = new Array[A](count)
        var outIdx = 0
        j = 0
        while j < n do
          var posArr = arr.offset
          var posMask = mask.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % arr.shape(k)
            posArr += coord * arr.strides(k)
            posMask += coord * mask.strides(k)
            k += 1
          end while
          if mask.data(posMask) then
            out(outIdx) = arr.data(posArr)
            outIdx += 1
          end if
          j += 1
        end while
        mkNDArray(out, Array(count), Array(1), 0)
      end if
    end apply

    /** Set elements where mask is true to `value`. Mutates arr.data in-place. Mask must have the same shape as arr, and
      * arr must be contiguous.
      */
    inline def update(mask: NDArray[Boolean], value: A)(using inline bc: BoundsCheck): Unit =
      inline if bc then
        if !sameShape(arr.shape, mask.shape) then
          throw ShapeMismatchException(
            s"Boolean mask assignment requires arr and mask to have the same shape: [${arr.shape.mkString(",")}] vs [${mask.shape.mkString(",")}]."
          )
        end if
        if !arr.isContiguous then throw InvalidNDArray("Boolean mask assignment requires a contiguous NDArray")
        end if
      end if
      if arr.isColMajor && mask.isColMajor then
        var i = 0
        while i < mask.data.length do
          if mask.data(i) then arr.data(i) = value
          end if
          i += 1
        end while
      else
        val n = arr.numel
        val ndim = arr.ndim
        val cumProd = colMajorStrides(arr.shape)
        var j = 0
        while j < n do
          var posArr = arr.offset
          var posMask = mask.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % arr.shape(k)
            posArr += coord * arr.strides(k)
            posMask += coord * mask.strides(k)
            k += 1
          end while
          if mask.data(posMask) then arr.data(posArr) = value
          end if
          j += 1
        end while
      end if
    end update

  end extension

end NDArrayBooleanIndexing
