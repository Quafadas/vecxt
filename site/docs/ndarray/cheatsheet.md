# NDArray Cheatsheet

## vecxt NDArray vs NumPy vs MATLAB

This cheatsheet compares common N-dimensional array operations across vecxt (Scala 3), NumPy (Python), and MATLAB. Scala 3.7.3+ is assumed for vecxt.

```scala
//> using scala 3.7.3 // or greater
import vecxt.all.{*, given}
import vecxt.BoundsCheck.DoBoundsCheck.yes
```

## Construction

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| 1D from data | `NDArray.fromArray(Array(1.0, 2.0, 3.0))` | `np.array([1., 2., 3.])` | `[1 2 3]` |
| N-D from flat data + shape | `NDArray(data, Array(2, 3, 4))` | `np.array(data).reshape(2, 3, 4)` | `reshape(data, [2 3 4])` |
| Array of zeros | `NDArray.zeros[Double](Array(2, 3, 4))` | `np.zeros((2, 3, 4))` | `zeros(2,3,4)` |
| Array of ones | `NDArray.ones[Double](Array(2, 3, 4))` | `np.ones((2, 3, 4))` | `ones(2,3,4)` |
| Fill with value | `NDArray.fill(Array(2, 3), 7.0)` | `np.full((2, 3), 7.0)` | `repmat(7, [2 3])` |
| With explicit strides | `NDArray(data, shape, strides, offset)` | `np.lib.stride_tricks.as_strided(...)` | N/A |

**Layout:** vecxt NDArray defaults to **column-major (Fortran-order)** — the first index varies fastest in memory. This matches the existing `Matrix[A]` convention and is BLAS-native.

## Properties

| Property | vecxt | NumPy | MATLAB |
|----------|-------|-------|--------|
| Number of dimensions | `arr.ndim` | `a.ndim` | `ndims(a)` |
| Shape | `arr.shape` | `a.shape` | `size(a)` |
| Total elements | `arr.numel` | `a.size` | `numel(a)` |
| Strides | `arr.strides` | `a.strides` | N/A |
| Memory offset | `arr.offset` | N/A | N/A |
| Layout string | `arr.layout` | N/A | N/A |
| Is column-major | `arr.isColMajor` | `a.flags['F_CONTIGUOUS']` | N/A |
| Is row-major | `arr.isRowMajor` | `a.flags['C_CONTIGUOUS']` | N/A |
| Is contiguous | `arr.isContiguous` | `a.flags['C_CONTIGUOUS'] or a.flags['F_CONTIGUOUS']` | N/A |

## Indexing

NDArray uses **0-based indexing**. Element at `(i₀, i₁, …, iₙ₋₁)` lives at `offset + Σ(iₖ × stridesₖ)`.

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Read 1D element | `arr(i)` | `a[i]` | `a(i+1)` |
| Read 2D element | `arr(row, col)` | `a[row, col]` | `a(row+1, col+1)` |
| Read 3D element | `arr(i, j, k)` | `a[i, j, k]` | `a(i+1,j+1,k+1)` |
| Read 4D element | `arr(i, j, k, l)` | `a[i, j, k, l]` | `a(i+1,j+1,k+1,l+1)` |
| Read via index array | `arr(Array(i, j, k))` | `a[i, j, k]` | `a(i+1,j+1,k+1)` |
| Write 1D element | `arr.update(i, v)` | `a[i] = v` | `a(i+1) = v` |
| Write 2D element | `arr.update(i, j, v)` | `a[i, j] = v` | `a(i+1,j+1) = v` |

## Slicing and Views

All slice/view operations return zero-copy views sharing the backing array — mutation is visible in the original.

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Keep all elements (1D) | `arr(::)` | `a[:]` | `a(:)` |
| Range slice | `arr(1 until 4)` | `a[1:4]` | `a(2:4)` |
| Slice along dimension `d` | `arr.slice(d, start, end)` | `np.take(a, range(start,end), axis=d)` | N/A |
| Multi-dim: keep all rows, slice cols | `arr(::, 1 until 3)` | `a[:, 1:3]` | `a(:, 2:3)` |
| Gather rows by index array | `arr(Array(0, 2), ::)` | `a[[0, 2], :]` | `a([1 3], :)` |
| Transpose (2D) | `arr.T` | `a.T` | `a.'` |
| N-D permutation | `arr.transpose(Array(2,0,1))` | `np.transpose(a, (2,0,1))` | `permute(a, [3 1 2])` |

## Reshaping and Shape Manipulation

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Reshape | `arr.reshape(Array(4, 3))` | `a.reshape(4, 3)` | `reshape(a, 4, 3)` |
| Flatten to 1D | `arr.flatten` | `a.flatten()` | `a(:)` |
| Remove size-1 dims | `arr.squeeze` | `a.squeeze()` | `squeeze(a)` |
| Remove specific dim | `arr.squeeze(0)` | `a.squeeze(axis=0)` | N/A |
| Insert size-1 dim | `arr.unsqueeze(0)` | `np.expand_dims(a, 0)` | N/A |
| Alias for unsqueeze | `arr.expandDims(0)` | `np.expand_dims(a, 0)` | N/A |
| To flat `Array[A]` | `arr.toArray` | `a.ravel()` | `a(:)` |

**Zero-copy vs copy:**
- `reshape` on a contiguous array returns a zero-copy view; on a non-contiguous array (e.g. transposed) it copies.
- `flatten` on a contiguous array is zero-copy; otherwise copies.
- `squeeze` / `unsqueeze` are always zero-copy.

## Element-wise Operations (`NDArray[Double]`)

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Addition | `a + b` | `a + b` | `a + b` |
| Subtraction | `a - b` | `a - b` | `a - b` |
| Multiplication | `a * b` | `a * b` | `a .* b` |
| Division | `a / b` | `a / b` | `a ./ b` |
| Scalar addition | `a + 2.0` | `a + 2.0` | `a + 2` |
| Scalar subtraction | `a - 2.0` | `a - 2.0` | `a - 2` |
| Scalar multiply | `a * 3.0` or `3.0 * a` | `a * 3` | `a * 3` |
| Scalar divide | `a / 3.0` | `a / 3` | `a / 3` |
| Scalar divide (left) | `6.0 / a` | `6 / a` | `6 ./ a` |

**Shape requirement:** Binary ops require **same shape**. Use `broadcastTo` or `broadcastPair` to align shapes first.

## Unary Operations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Negation | `a.neg` | `-a` | `-a` |
| Absolute value | `a.abs` | `np.abs(a)` | `abs(a)` |
| Exponential | `a.exp` | `np.exp(a)` | `exp(a)` |
| Natural log | `a.log` | `np.log(a)` | `log(a)` |
| Square root | `a.sqrt` | `np.sqrt(a)` | `sqrt(a)` |
| Hyperbolic tangent | `a.tanh` | `np.tanh(a)` | `tanh(a)` |
| Sigmoid | `a.sigmoid` | `1 / (1 + np.exp(-a))` | `1 ./ (1 + exp(-a))` |

## In-place Operations

In-place ops mutate the array in-place. The array must be **contiguous**; shapes must match.

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| In-place add | `a += b` | `a += b` | N/A |
| In-place subtract | `a -= b` | `a -= b` | N/A |
| In-place multiply | `a *= b` | `a *= b` | N/A |
| In-place divide | `a /= b` | `a /= b` | N/A |
| In-place scalar add | `a += 2.0` | `a += 2.0` | N/A |
| In-place scalar mul | `a *= 3.0` | `a *= 3.0` | N/A |

## Comparison Operations

Comparison ops return `NDArray[Boolean]` of the same shape.

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Greater than (scalar) | `a > 2.0` | `a > 2.0` | `a > 2` |
| Less than (scalar) | `a < 2.0` | `a < 2.0` | `a < 2` |
| Greater or equal | `a >= 2.0` | `a >= 2.0` | `a >= 2` |
| Less or equal | `a <= 2.0` | `a <= 2.0` | `a <= 2` |
| Equal | `a =:= 2.0` | `a == 2.0` | `a == 2` |
| Not equal | `a !:= 2.0` | `a != 2.0` | `a ~= 2` |
| Greater than (array) | `a > b` | `a > b` | `a > b` |
| Equal (array) | `a =:= b` | `a == b` | `a == b` |

## Broadcasting

Broadcasting in vecxt is **explicit** (unlike NumPy's implicit broadcasting). Use `broadcastTo` or `broadcastPair` before binary ops.

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Broadcast to target shape | `a.broadcastTo(Array(4, 3))` | N/A (implicit) | N/A (implicit) |
| Broadcast pair to common shape | `broadcastPair(a, b)` | N/A (implicit) | N/A (implicit) |
| Compute broadcast shape | `broadcastShape(s1, s2)` | `np.broadcast_shapes(s1, s2)` | N/A |

Broadcasting follows NumPy semantics:
1. Shapes are right-aligned
2. Dimensions are compatible if equal or one of them is 1
3. A dimension of 1 expands via stride-0 (zero-copy view)

## Notes

- **vecxt**: Column-major (Fortran-order) layout by default. N-dimensional generalisation of `Matrix[A]`.
- **NumPy**: Row-major (C-order) by default. N-dimensional.
- **MATLAB**: Column-major by default. N-dimensional.
- **Indexing**: vecxt and NumPy use 0-based indexing; MATLAB uses 1-based.
- **Broadcasting**: vecxt is explicit; NumPy and MATLAB are implicit.
- **Views**: slice, reshape (contiguous), T, transpose, squeeze, unsqueeze, flatten (contiguous) all return zero-copy views sharing the backing `Array[A]`.
- **Mutation**: Views share backing data — mutation through a view is visible in the original.
