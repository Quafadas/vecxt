# Linear Algebra Cheatsheet: vecxt vs NumPy vs MATLAB

This cheatsheet compares common linear algebra operations across vecxt (Scala 3), NumPy (Python), and MATLAB. Scala 3.7.3+ is assumed for vecxt.

```scala
//> using scala 3.7.3 // or greater
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import narr.*

```

## Array/Vector Creation and Basic Operations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Create a 1D array | `NArray(1.0, 2.0, 3.0)` | `np.array([1., 2., 3.])` | `[1 2 3]` |
| Create a 2D array/matrix | `Matrix(NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)` | `np.array([[1., 2., 3.], [4., 5., 6.]])` | `[1 2 3; 4 5 6]` |
| Array of zeros | `Matrix.zeros((3, 4))` | `np.zeros((3, 4))` | `zeros(3,4)` |
| Array of ones | `Matrix.ones((3, 4))` | `np.ones((3, 4))` | `ones(3,4)` |
| Identity matrix | `Matrix.eye(3)` | `np.eye(3)` | `eye(3)` |
| Get array dimensions | `m.rows, m.cols` or `m.shape` | `a.shape` | `size(a)` |
| Number of elements | `m.numel` | `a.size` | `numel(a)` |

## Indexing and Slicing

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Access single element | `m(i, j)` | `a[i, j]` | `a(i,j)` |
| Access last element (vector) | `vec.last` | `a[-1]` | `a(end)` |
| Access entire row | `m.row(i)` | `a[i]` or `a[i, :]` | `a(i,:)` |
| Access entire column | `m.col(j)` | `a[:, j]` | `a(:,j)` |
| First 5 rows | `m(0 until 5, ::)` | `a[:5]` or `a[0:5, :]` | `a(1:5,:)` |
| Last 5 rows | `m((m.rows-5) until m.rows, ::)` | `a[-5:]` | `a(end-4:end,:)` |
| Submatrix | `m(0 to 3, 4 to 9)` | `a[0:3, 4:9]` | `a(1:3,5:9)` |
| Reverse rows | `m((m.rows -1 until 0 by -1), ::)` | `a[::-1,:]` | `a(end:-1:1,:)` or `flipud(a)` |
| Transpose | `m.transpose` or `m.T` | `a.transpose()` or `a.T` | `a.'` |

## Element-wise Operations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Element-wise addition | `m + n` or `m += n` or `m +:+ n` | `a + b` | `a + b` |
| Element-wise subtraction | `m - n` or `m -= n` or `m -:- n` | `a - b` | `a - b` |
| Element-wise multiply | ` m * n or m.hadamard(n)` | `a * b` | `a .* b` |
| Element-wise divide | `m /:/ n` | `a / b` | `a ./ b` |
| Scalar addition | `m + scalar` | `a + scalar` | `a + scalar` |
| Scalar multiplication | `m * scalar` or `scalar * m` | `a * scalar` | `a * scalar` |
| Scalar division | `m / scalar` | `a / scalar` | `a / scalar` |
| Element-wise power | `vec ** 3.0` | `a**3` | `a.^3` |
| Negate | `-vec` | `-a` | `-a` |
| Absolute value | `vec.abs` | `np.abs(a)` | `abs(a)` |
| Clip/clamp values | `vec.clamp(min, max)` | `np.clip(a, min, max)` | `max(min, min(a, max))` |


## Matrix Operations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Matrix multiplication | `m @@ n` or `m.matmul(n)` | `a @ b` | `a * b` |
| Matrix-vector multiply | `m @@ v` | `a @ v` | `a * v` |
| Dot product | `vec.dot(vec2)` | `np.dot(a, b)` or `a @ b` | `dot(a,b)` |
| Determinant | `m.det` | `np.linalg.det(a)` | `det(a)` |
| Matrix inverse | `m.inv` | `np.linalg.inv(a)` | `inv(a)` |
| SVD | `val (U, S, Vt) = svd(m)` | `U, S, Vh = np.linalg.svd(a)` | `[U,S,V]=svd(a)` |
| Pseudo-inverse | `pinv(m)` | `np.linalg.pinv(a)` | `pinv(a)` |
| Matrix rank | `m.rank` or `rank(m)` | `np.linalg.matrix_rank(a)` | `rank(a)` |
| Solve linear system | ??? | `np.linalg.solve(a, b)` | `a\b` |
| Eigenvalues/vectors | `eig(m)` | `D, V = np.linalg.eig(a)` | `[V,D]=eig(a)` |
| Cholesky decomposition | `cholesky(m)` | `np.linalg.cholesky(a)` | `chol(a)` |
| QR decomposition | ??? | `Q, R = np.linalg.qr(a)` | `[Q,R]=qr(a,0)` |
| LU decomposition | ??? | `P, L, U = scipy.linalg.lu(a)` | `[L,U,P]=lu(a)` |

## Reductions and Aggregations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Sum all elements | `m.sum` or `vec.sumSIMD` | `a.sum()` | `sum(a(:))` |
| Sum along rows | `m.sum(0)` or `m.sum(Dimension.Rows)` | `a.sum(axis=0)` | `sum(a)` |
| Sum along columns | `m.sum(1)` or `m.sum(Dimension.Cols)` | `a.sum(axis=1)` | `sum(a,2)` |
| Mean | `m.mean` | `a.mean()` | `mean(a(:))` |
| Max | `m.raw.max` or `vec.max` | `a.max()` | `max(a(:))` |
| Max along rows | `m.max(Dimension.Rows)` | `a.max(axis=0)` | `max(a)` |
| Max along columns | `m.max(Dimension.Cols)` | `a.max(axis=1)` | `max(a,[],2)` |
| Element-wise max | `m.maximum(b)` | `np.maximum(a, b)` | `max(a,b)` |
| Min | `m.raw.min` or `vec.min` | `a.min()` | `min(a(:))` |
| Min along axis | `m.min(Dimension.Rows)` or `m.min(Dimension.Cols)` | `a.min(axis=0)` | `min(a)` |
| Argmax | `vec.argmax` | `a.argmax()` | `[~,idx]=max(a(:))` |
| Argmin | `vec.argmin` | `a.argmin()` | `[~,idx]=min(a(:))` |

## Norms and Distances

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| L2 norm (vector) | `vec.norm` | `np.linalg.norm(v)` | `norm(v)` |
| Frobenius norm | `mat.norm` | `np.linalg.norm(a)` | `norm(a,'fro')` |
| Cosine similarity | `cosineSimilarity(v1, v2)` | `np.dot(a,b)/(np.linalg.norm(a)*np.linalg.norm(b))` | `dot(a,b)/(norm(a)*norm(b))` |

## Mathematical Functions (Element-wise)

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Exponential | `vec.exp` or `m.exp` | `np.exp(a)` | `exp(a)` |
| Natural logarithm | `vec.log` or `m.log` | `np.log(a)` | `log(a)` |
| Log base 10 | `vec.log10` | `np.log10(a)` | `log10(a)` |
| Square root | `vec.sqrt` or `m.sqrt` | `np.sqrt(a)` | `sqrt(a)` |
| Sine | `vec.sin` or `m.sin` | `np.sin(a)` | `sin(a)` |
| Cosine | `vec.cos` or `m.cos` | `np.cos(a)` | `cos(a)` |
| Tangent | `vec.tan` | `np.tan(a)` | `tan(a)` |
| Arcsine | `vec.asin` | `np.arcsin(a)` | `asin(a)` |
| Arccosine | `vec.acos` | `np.arccos(a)` | `acos(a)` |
| Arctangent | `vec.atan` | `np.arctan(a)` | `atan(a)` |
| Hyperbolic sine | `vec.sinh` | `np.sinh(a)` | `sinh(a)` |
| Hyperbolic cosine | `vec.cosh` | `np.cosh(a)` | `cosh(a)` |
| Hyperbolic tangent | `vec.tanh` | `np.tanh(a)` | `tanh(a)` |



In vecxt, each of these function has an "in-place" counterpart that returns unit. e.g. `vec.sin!` which modifies the original vector in place. This may be helpful for performance reasons to avoid memory allocation.

Such operations can also be called via `tan(vec)`, `exp(matrix)`, etc.

## Logical Operations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Element-wise AND | `a && b` | `np.logical_and(a,b)` | `a & b` |
| Element-wise OR | `a \|\| b` | `np.logical_or(a,b)` | `a \| b` |
| Element-wise NOT | `not(a)` | `np.logical_not(a)` | `~a` |
| Element-wise comparison > | `a > b` or `a.gt(b)` | `a > b` | `a > b` |
| Element-wise comparison < | `a < b` or `a.lt(b)` | `a < b` | `a < b` |
| Element-wise comparison >= | `a >= b` or `a.gte(b)` | `a >= b` | `a >= b` |
| Element-wise comparison <= | `a <= b` or `a.lte(b)` | `a <= b` | `a <= b` |
| Element-wise equality | `a =:= b` | `a == b` | `a == b` |
| Element-wise inequality | `a !:= b` | `a != b` | `a ~= b` |
| Find indices where true | `idx.logicalIdx(...)` | `np.nonzero(a > 0.5)` | `find(a > 0.5)` |
| Boolean indexing | `a(a > 2.0)` | `a[a > 0.5]` | `a(a > 0.5)` |
| Count true values | `(a > 2.0).trues` | `np.sum(a > 0.5)` | `sum(a > 0.5)` |

## Array Manipulation

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Extract diagonal | `m.diag` | `np.diag(a)` | `diag(a)` |
| Create diagonal matrix | `Matrix.diag(a)` | `np.diag(v)` | `diag(v,0)` |
| Unique values | `vec.unique` | `np.unique(a)` | `unique(a)` |
| Sort | `narr.sort(vec)()` | `np.sort(a)` | `sort(a)` |

## Special Operations

| Operation | vecxt | NumPy | MATLAB |
|-----------|-------|-------|--------|
| Copy array | `vec.clone()` or `narr.copy(vec)` | `a.copy()` | `b = a` (MATLAB copies by value) |
| Copy assignment (view vs copy) | `val b = a` (reference) | `b = a` (reference) | `b = a` (copy) |
| Linspace | `linspace(1,3,4)` | `np.linspace(1, 3, 4)` | `linspace(1,3,4)` |
| Increments (diff) | `vec.increments` | `np.diff(a)` | `diff(a)` |
| Log-sum-exp | `vec.logSumExp` | `scipy.special.logsumexp(a)` | N/A |
| Product except self| `vec.productExceptSelf` | `np.prod(a) / a` | `prod(a) ./ a` |

## Notes

- **vecxt**: Cross-platform (JVM, JS, Native) Scala 3 library optimizing for SIMD on JVM
- **NumPy**: Python's numerical computing library
- **MATLAB**: Commercial numerical computing environment
- **???**: Feature not yet implemented or discovered in vecxt
- **Indexing**: vecxt and NumPy use 0-based indexing; MATLAB uses 1-based indexing
- **Copy semantics**: vecxt and NumPy use reference semantics by default (need explicit copy); MATLAB copies by value
- **Column vs Row major**: vecxt uses column-major by default, NumPy uses row-major by default
- **In-place operations**: vecxt supports in-place operations with `!` suffix (e.g., `abs!`, `exp!`)
