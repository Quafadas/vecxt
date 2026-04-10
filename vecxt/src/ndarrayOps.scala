package vecxt

import scala.reflect.ClassTag

import vecxt.BoundsCheck.BoundsCheck
import vecxt.IntArraysX.*
import vecxt.ndarray.*
import vecxt.rangeExtender.MatrixRange.*

object ndarrayOps:

  inline def sameAndContiguousMemoryLayout(a: NDArray[?], b: NDArray[?]): Boolean =
    (a.isColMajor && b.isColMajor) || (a.isRowMajor && b.isRowMajor)

  extension [A](arr: NDArray[A])

    // ── Element read (apply) ────────────────────────────────────────────────

    /** Read a single element from a 1D NDArray. */
    inline def apply(i0: Int)(using inline bc: BoundsCheck): A =
      inline if bc then
        if arr.ndim != 1 then throw InvalidNDArray(s"Rank mismatch: expected ndim=1, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
      end if
      arr.data(arr.offset + i0 * arr.strides(0))
    end apply

    /** Read a single element from a 2D NDArray (col-major: first index = row, second = column). */
    inline def apply(i0: Int, i1: Int)(using inline bc: BoundsCheck): A =
      inline if bc then
        if arr.ndim != 2 then throw InvalidNDArray(s"Rank mismatch: expected ndim=2, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
        if i1 < 0 || i1 >= arr.shape(1) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i1 out of bounds for dim 1 of size ${arr.shape(1)}"
          )
        end if
      end if
      arr.data(arr.offset + i0 * arr.strides(0) + i1 * arr.strides(1))
    end apply

    /** Read a single element from a 3D NDArray. */
    inline def apply(i0: Int, i1: Int, i2: Int)(using inline bc: BoundsCheck): A =
      inline if bc then
        if arr.ndim != 3 then throw InvalidNDArray(s"Rank mismatch: expected ndim=3, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
        if i1 < 0 || i1 >= arr.shape(1) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i1 out of bounds for dim 1 of size ${arr.shape(1)}"
          )
        end if
        if i2 < 0 || i2 >= arr.shape(2) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i2 out of bounds for dim 2 of size ${arr.shape(2)}"
          )
        end if
      end if
      arr.data(arr.offset + i0 * arr.strides(0) + i1 * arr.strides(1) + i2 * arr.strides(2))
    end apply

    /** Read a single element from a 4D NDArray. */
    inline def apply(i0: Int, i1: Int, i2: Int, i3: Int)(using inline bc: BoundsCheck): A =
      inline if bc then
        if arr.ndim != 4 then throw InvalidNDArray(s"Rank mismatch: expected ndim=4, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
        if i1 < 0 || i1 >= arr.shape(1) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i1 out of bounds for dim 1 of size ${arr.shape(1)}"
          )
        end if
        if i2 < 0 || i2 >= arr.shape(2) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i2 out of bounds for dim 2 of size ${arr.shape(2)}"
          )
        end if
        if i3 < 0 || i3 >= arr.shape(3) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i3 out of bounds for dim 3 of size ${arr.shape(3)}"
          )
        end if
      end if
      arr.data(
        arr.offset + i0 * arr.strides(0) + i1 * arr.strides(1) + i2 * arr.strides(2) + i3 * arr.strides(3)
      )
    end apply

    /** Read a single element using an Array of indices (N-D general case). */
    inline def apply(indices: Array[Int])(using inline bc: BoundsCheck): A =
      indexNDArrayCheck(arr, indices)
      var pos = arr.offset
      var k = 0
      while k < indices.length do
        pos += indices(k) * arr.strides(k)
        k += 1
      end while
      arr.data(pos)
    end apply

    /** Read the single element of a 0-d NDArray. */
    inline def scalar(using inline bc: BoundsCheck): A =
      inline if bc then
        if arr.ndim != 0 then throw InvalidNDArray(s"scalar accessor requires ndim=0, got ndim=${arr.ndim}")
        end if
      end if
      arr.data(arr.offset)
    end scalar

    // ── Element write (update) ──────────────────────────────────────────────

    /** Write a single element in a 1D NDArray. */
    inline def update(i0: Int, value: A)(using inline bc: BoundsCheck): Unit =
      inline if bc then
        if arr.ndim != 1 then throw InvalidNDArray(s"Rank mismatch: expected ndim=1, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
      end if
      arr.data(arr.offset + i0 * arr.strides(0)) = value
    end update

    /** Write a single element in a 2D NDArray. */
    inline def update(i0: Int, i1: Int, value: A)(using inline bc: BoundsCheck): Unit =
      inline if bc then
        if arr.ndim != 2 then throw InvalidNDArray(s"Rank mismatch: expected ndim=2, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
        if i1 < 0 || i1 >= arr.shape(1) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i1 out of bounds for dim 1 of size ${arr.shape(1)}"
          )
        end if
      end if
      arr.data(arr.offset + i0 * arr.strides(0) + i1 * arr.strides(1)) = value
    end update

    /** Write a single element in a 3D NDArray. */
    inline def update(i0: Int, i1: Int, i2: Int, value: A)(using inline bc: BoundsCheck): Unit =
      inline if bc then
        if arr.ndim != 3 then throw InvalidNDArray(s"Rank mismatch: expected ndim=3, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
        if i1 < 0 || i1 >= arr.shape(1) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i1 out of bounds for dim 1 of size ${arr.shape(1)}"
          )
        end if
        if i2 < 0 || i2 >= arr.shape(2) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i2 out of bounds for dim 2 of size ${arr.shape(2)}"
          )
        end if
      end if
      arr.data(arr.offset + i0 * arr.strides(0) + i1 * arr.strides(1) + i2 * arr.strides(2)) = value
    end update

    /** Write a single element in a 4D NDArray. */
    inline def update(i0: Int, i1: Int, i2: Int, i3: Int, value: A)(using inline bc: BoundsCheck): Unit =
      inline if bc then
        if arr.ndim != 4 then throw InvalidNDArray(s"Rank mismatch: expected ndim=4, got ndim=${arr.ndim}")
        end if
        if i0 < 0 || i0 >= arr.shape(0) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i0 out of bounds for dim 0 of size ${arr.shape(0)}"
          )
        end if
        if i1 < 0 || i1 >= arr.shape(1) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i1 out of bounds for dim 1 of size ${arr.shape(1)}"
          )
        end if
        if i2 < 0 || i2 >= arr.shape(2) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i2 out of bounds for dim 2 of size ${arr.shape(2)}"
          )
        end if
        if i3 < 0 || i3 >= arr.shape(3) then
          throw new java.lang.IndexOutOfBoundsException(
            s"Index $i3 out of bounds for dim 3 of size ${arr.shape(3)}"
          )
        end if
      end if
      arr.data(
        arr.offset + i0 * arr.strides(0) + i1 * arr.strides(1) + i2 * arr.strides(2) + i3 * arr.strides(3)
      ) = value
    end update

    /** Write a single element using an Array of indices (N-D general case). */
    inline def update(indices: Array[Int], value: A)(using inline bc: BoundsCheck): Unit =
      indexNDArrayCheck(arr, indices)
      var pos = arr.offset
      var k = 0
      while k < indices.length do
        pos += indices(k) * arr.strides(k)
        k += 1
      end while
      arr.data(pos) = value
    end update

    /** Write the single element of a 0-d NDArray. */
    inline def setScalar(value: A)(using inline bc: BoundsCheck): Unit =
      inline if bc then
        if arr.ndim != 0 then throw InvalidNDArray(s"setScalar requires ndim=0, got ndim=${arr.ndim}")
        end if
      end if
      arr.data(arr.offset) = value
    end setScalar

    // ── Slice (view, no copy) ───────────────────────────────────────────────

    /** Returns a view of `arr` with dimension `dim` restricted to `[start, end)`.
      *
      * The returned NDArray shares the backing `data` — mutation through the view is visible in the original.
      */
    inline def slice(dim: Int, start: Int, end: Int)(using inline bc: BoundsCheck): NDArray[A] =
      inline if bc then
        if dim < 0 || dim >= arr.ndim then throw InvalidNDArray(s"Dimension $dim out of range [0, ${arr.ndim})")
        end if
        if start < 0 || start >= arr.shape(dim) then
          throw InvalidNDArray(s"Start $start out of range [0, ${arr.shape(dim)})")
        end if
        if end <= start || end > arr.shape(dim) then
          throw InvalidNDArray(
            s"End $end invalid: must satisfy start ($start) < end <= dim_size (${arr.shape(dim)})"
          )
        end if
      end if
      val newShape = arr.shape.clone()
      newShape(dim) = end - start
      val newOffset = arr.offset + start * arr.strides(dim)
      mkNDArray(arr.data, newShape, arr.strides.clone(), newOffset)
    end slice

    // ── Transpose / T ──────────────────────────────────────────────────────

    /** Swap the two axes of a 2D NDArray (zero-copy view). */
    inline def T(using inline bc: BoundsCheck): NDArray[A] =
      inline if bc then if arr.ndim != 2 then throw InvalidNDArray(s"T requires ndim=2, got ndim=${arr.ndim}")
      end if
      mkNDArray(
        arr.data,
        Array(arr.shape(1), arr.shape(0)),
        Array(arr.strides(1), arr.strides(0)),
        arr.offset
      )
    end T

    /** Permute the dimensions of the NDArray according to `perm` (zero-copy view).
      *
      * `perm` must be a permutation of `0 until ndim`.
      */
    inline def transpose(perm: Array[Int])(using inline bc: BoundsCheck): NDArray[A] =
      inline if bc then
        if perm.length != arr.ndim then
          throw InvalidNDArray(
            s"Permutation length (${perm.length}) must match ndim (${arr.ndim})"
          )
        end if
        val seen = new Array[Boolean](arr.ndim)
        var k = 0
        while k < perm.length do
          if perm(k) < 0 || perm(k) >= arr.ndim then
            throw InvalidNDArray(s"Permutation value ${perm(k)} out of range [0, ${arr.ndim})")
          end if
          if seen(perm(k)) then throw InvalidNDArray(s"Permutation has duplicate value ${perm(k)}")
          end if
          seen(perm(k)) = true
          k += 1
        end while
      end if
      val newShape = new Array[Int](arr.ndim)
      val newStrides = new Array[Int](arr.ndim)
      var k = 0
      while k < arr.ndim do
        newShape(k) = arr.shape(perm(k))
        newStrides(k) = arr.strides(perm(k))
        k += 1
      end while
      mkNDArray(arr.data, newShape, newStrides, arr.offset)
    end transpose

    // ── Reshape ────────────────────────────────────────────────────────────

    /** Returns an NDArray with a different shape but the same total number of elements.
      *
      * If the array is already contiguous (col-major or row-major), returns a zero-copy view. Otherwise, a contiguous
      * copy is made first.
      */
    inline def reshape(newShape: Array[Int])(using inline bc: BoundsCheck, ct: ClassTag[A]): NDArray[A] =
      inline if bc then
        shapeCheck(newShape)
        if shapeProduct(newShape) != arr.numel then
          throw InvalidNDArray(
            s"Cannot reshape array of ${arr.numel} elements to shape [${newShape.mkString(",")}]"
          )
        end if
      end if
      if arr.isColMajor then
        // Fast path: isColMajor guarantees offset==0, dense col-major strides, and data.length==numel,
        // so we can safely reuse the backing array and assign new col-major strides.
        mkNDArray(arr.data, newShape.clone(), colMajorStrides(newShape), 0)
      else
        // Copy to contiguous col-major order first (handles row-major and strided views).
        val contiguous = arr.toArray
        mkNDArray(contiguous, newShape.clone(), colMajorStrides(newShape), 0)
      end if
    end reshape

    // ── Squeeze / Unsqueeze ────────────────────────────────────────────────

    /** Remove all dimensions of size 1 (zero-copy view). */
    inline def squeeze: NDArray[A] =
      var count = 0
      var i = 0
      while i < arr.ndim do
        if arr.shape(i) != 1 then count += 1
        end if
        i += 1
      end while
      val newShape = new Array[Int](count)
      val newStrides = new Array[Int](count)
      var j = 0
      i = 0
      while i < arr.ndim do
        if arr.shape(i) != 1 then
          newShape(j) = arr.shape(i)
          newStrides(j) = arr.strides(i)
          j += 1
        end if
        i += 1
      end while
      mkNDArray(arr.data, newShape, newStrides, arr.offset)
    end squeeze

    /** Remove dimension `dim`, which must have size 1 (zero-copy view). */
    inline def squeeze(dim: Int)(using inline bc: BoundsCheck): NDArray[A] =
      inline if bc then
        if dim < 0 || dim >= arr.ndim then throw InvalidNDArray(s"Dimension $dim out of range [0, ${arr.ndim})")
        end if
        if arr.shape(dim) != 1 then
          throw InvalidNDArray(
            s"Cannot squeeze dimension $dim of size ${arr.shape(dim)}: size must be 1"
          )
        end if
      end if
      val newShape = new Array[Int](arr.ndim - 1)
      val newStrides = new Array[Int](arr.ndim - 1)
      var j = 0
      var i = 0
      while i < arr.ndim do
        if i != dim then
          newShape(j) = arr.shape(i)
          newStrides(j) = arr.strides(i)
          j += 1
        end if
        i += 1
      end while
      mkNDArray(arr.data, newShape, newStrides, arr.offset)
    end squeeze

    /** Insert a size-1 dimension at position `dim` (zero-copy view).
      *
      * `dim` must be in `[0, ndim]`.
      */
    inline def unsqueeze(dim: Int)(using inline bc: BoundsCheck): NDArray[A] =
      inline if bc then
        if dim < 0 || dim > arr.ndim then throw InvalidNDArray(s"Dimension $dim out of range [0, ${arr.ndim}]")
      end if
      val newNdim = arr.ndim + 1
      val newShape = new Array[Int](newNdim)
      val newStrides = new Array[Int](newNdim)
      var i = 0
      while i < dim do
        newShape(i) = arr.shape(i)
        newStrides(i) = arr.strides(i)
        i += 1
      end while
      newShape(dim) = 1
      // Stride=1 for a size-1 dimension is always safe: we never move more than 0
      // steps in that dimension, so the stride value is irrelevant for data access.
      newStrides(dim) = 1
      i = dim + 1
      while i < newNdim do
        newShape(i) = arr.shape(i - 1)
        newStrides(i) = arr.strides(i - 1)
        i += 1
      end while
      mkNDArray(arr.data, newShape, newStrides, arr.offset)
    end unsqueeze

    /** Alias for `unsqueeze`. */
    inline def expandDims(dim: Int)(using inline bc: BoundsCheck): NDArray[A] =
      arr.unsqueeze(dim)

    // ── Flatten ────────────────────────────────────────────────────────────

    /** Returns a 1D view of the array.
      *
      * If the array is contiguous (col- or row-major, offset=0), returns a zero-copy view. Otherwise copies the
      * elements into a fresh contiguous array first (in column-major order).
      */
    inline def flatten(using ct: ClassTag[A]): NDArray[A] =
      if arr.isContiguous then mkNDArray(arr.data, Array(arr.numel), Array(1), arr.offset)
      else
        val out = arr.toArray
        mkNDArray(out, Array(arr.numel), Array(1), 0)

    // ── Multi-dimensional slice/select ─────────────────────────────────────

    /** Multi-dimensional slice/select. One selector per dimension.
      *
      *   - `::` → keep entire dimension (zero-copy)
      *   - `Range` (e.g. `1 until 3`) → contiguous slice (zero-copy if all selectors are `::` or step-1 `Range`)
      *   - `Array[Int]` → gather (may require copy)
      *
      * Number of selectors must equal `arr.ndim`. Always validates bounds.
      */
    def apply(selectors: RangeExtender*)(using ct: ClassTag[A]): NDArray[A] =
      if selectors.length != arr.ndim then
        throw InvalidNDArray(
          s"Expected ${arr.ndim} selectors for ndim=${arr.ndim}, got ${selectors.length}"
        )
      end if

      // Validate bounds and determine if all selectors are contiguous
      var allContiguous = true
      var k = 0
      while k < arr.ndim do
        selectors(k) match
          case _: ::.type => // always contiguous, skip
          case r: Range   =>
            if r.start < 0 || r.end > arr.shape(k) then
              throw new java.lang.IndexOutOfBoundsException(
                s"Range ${r.start} until ${r.end} out of bounds for dim $k of size ${arr.shape(k)}"
              )
            end if
            if r.step != 1 then allContiguous = false
            end if
          case a: Array[Int] =>
            var i = 0
            while i < a.length do
              if a(i) < 0 || a(i) >= arr.shape(k) then
                throw new java.lang.IndexOutOfBoundsException(
                  s"Index ${a(i)} out of bounds for dim $k of size ${arr.shape(k)}"
                )
              end if
              i += 1
            end while
            if !a.contiguous then allContiguous = false
            end if
        end match
        k += 1
      end while

      // Compute output shape from selectors
      val newShape = new Array[Int](arr.ndim)
      k = 0
      while k < arr.ndim do
        newShape(k) = selectors(k) match
          case _: ::.type    => arr.shape(k)
          case r: Range      => r.length
          case a: Array[Int] => a.length
        k += 1
      end while

      if allContiguous then
        // Zero-copy view: adjust offset, keep strides
        var newOffset = arr.offset
        k = 0
        while k < arr.ndim do
          val start = selectors(k) match
            case _: ::.type    => 0
            case r: Range      => r.start
            case a: Array[Int] => if a.isEmpty then 0 else a(0)
          newOffset += start * arr.strides(k)
          k += 1
        end while
        mkNDArray(arr.data, newShape, arr.strides.clone(), newOffset)
      else
        // Copy/gather path: resolve each selector to Array[Int] and gather
        val resolved = new Array[Array[Int]](arr.ndim)
        k = 0
        while k < arr.ndim do
          resolved(k) = range(selectors(k), arr.shape(k))
          k += 1
        end while

        val outStrides = colMajorStrides(newShape)
        val n = shapeProduct(newShape)
        val out = new Array[A](n)

        var j = 0
        while j < n do
          var posIn = arr.offset
          k = 0
          while k < arr.ndim do
            val coord = (j / outStrides(k)) % newShape(k)
            posIn += resolved(k)(coord) * arr.strides(k)
            k += 1
          end while
          out(j) = arr.data(posIn)
          j += 1
        end while
        mkNDArray(out, newShape, outStrides, 0)
      end if
    end apply

    // ── toArray ────────────────────────────────────────────────────────────

    /** Materialise to a contiguous `Array[A]` in column-major order.
      *
      * If the array is already dense col-major with offset=0, returns a clone of the backing data. Otherwise iterates
      * via the stride formula.
      */
    inline def toArray(using ct: ClassTag[A]): Array[A] =
      if arr.isColMajor then arr.data.clone()
      else
        val result = new Array[A](arr.numel)
        val indices = new Array[Int](arr.ndim)
        var outPos = 0
        while outPos < arr.numel do
          var flatIdx = arr.offset
          var k = 0
          while k < arr.ndim do
            flatIdx += indices(k) * arr.strides(k)
            k += 1
          end while
          result(outPos) = arr.data(flatIdx)
          outPos += 1
          // increment odometer in col-major order (dim 0 varies fastest)
          var d = 0
          var carry = true
          while d < arr.ndim && carry do
            indices(d) += 1
            if indices(d) < arr.shape(d) then carry = false
            else
              indices(d) = 0
              d += 1
            end if
          end while
        end while
        result

  end extension

end ndarrayOps
