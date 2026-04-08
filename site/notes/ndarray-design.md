# NDArray Design Notes

## Critique of the Earlier Discussion

### What's right

1. **AD operates on whole tensors, not elements.** `Array[Jet[Double]]` is indeed catastrophic. `Jet[NDArray]` and `Node[NDArray]` are the correct shapes. This is the single most important insight.

2. **Runtime shapes over type-level dimension arithmetic.** Correct for a library targeting usability and cross-platform consistency. Type-level dimension encoding (`NDArray[3]`) leads to painful type-level arithmetic with reshape, broadcast, squeeze, and dynamic shapes. It doesn't compose well with Scala 3's type system without significant complexity.

3. **The interpreter pattern for AD.** Evaluating the same AST with different semantics (plain, forward AD, reverse AD) is clean and proven. Good separation of concerns.

4. **Contiguous `Array[Double]` as the memory substrate.** Correct for performance on all three platforms (JVM SIMD, JS typed arrays, Native CBLAS).

### What needs revision

#### 1. The `Tensor` type as proposed is too simplistic

```scala
// From the discussion:
case class Tensor(data: Array[Double], shape: Array[Int])
```

This ignores **strides**, **offsets**, and **memory layout detection** — all things vecxt's `Matrix[A]` already handles. The existing Matrix has:

- `raw: Array[A]`, `rows`, `cols`, `rowStride`, `colStride`, `offset`
- `isDenseColMajor`, `isDenseRowMajor`, `hasSimpleContiguousMemoryLayout`

An NDArray that can't represent views, transpositions, or slices without copying is a regression.

#### 2. "Tensor wraps NDArray" vs sharing a conceptual model

The discussion leans toward a single `Tensor` type that serves both as storage and as an AD computation node. These are fundamentally different concerns:

- **NDArray**: storage + shape + strides + element-wise and reduction ops. No gradient awareness.
- **Tensor** (AD context): NDArray + gradient tracking + computation graph participation.

Conflating them means every NDArray carries AD overhead (even when you're just doing data manipulation), or you end up with two parallel APIs.

#### 3. `opaque type Vector = Tensor` / `opaque type Matrix = Tensor` is problematic

This loses:
- `@specialized` (opaque types don't propagate specialization)
- The rich existing Matrix API
- Type-safe guarantees about dimensionality at construction time

Better: Matrix IS-A 2D NDArray (or wraps one with zero overhead), not an alias for an untyped Tensor.

#### 4. Element type genericity is missing

The discussion only mentions `Array[Double]`. Vecxt already supports `Matrix[Boolean]`, `Matrix[Int]`, and has `IntArrays`, `LongArrays`, `FloatArrays`. An NDArray should be `NDArray[A]` with specialization, not locked to Double.

#### 5. The MathAST / typeclass layer is a separate module concern

The AST, `Ops[A]` typeclass, and AD interpreters should NOT live in the NDArray module. NDArray is a data structure with operations. AD is a higher-level concern that *uses* NDArray. Mixing them creates circular dependencies and bloats the core.

---

## Revised Conceptual Model

### The shared abstraction: Strided N-dimensional Array

The core insight: **NDArray, Matrix, and (future) Tensor all share the same memory model:**

```
┌─────────────────────────────────────────────┐
│  Contiguous Array[A] in memory              │
│  ┌──────────────────────────────────────┐   │
│  │ offset ──► shaped window into data   │   │
│  │            via strides               │   │
│  └──────────────────────────────────────┘   │
└─────────────────────────────────────────────┘
```

Every N-dimensional view is defined by:
- `data: Array[A]` — the backing storage
- `shape: Array[Int]` — dimensions `[d₀, d₁, ..., dₙ₋₁]`
- `strides: Array[Int]` — memory step per dimension `[s₀, s₁, ..., sₙ₋₁]`
- `offset: Int` — start position in `data`

Element at index `(i₀, i₁, ..., iₙ₋₁)` lives at:

$$\text{offset} + \sum_{k=0}^{n-1} i_k \cdot s_k$$

This is exactly what `Matrix[A]` already does for n=2:
- `shape = [rows, cols]`
- `strides = [rowStride, colStride]`

### Type hierarchy

```
Array[A]          ← 1D, no wrapper needed (vecxt's existing pattern)
    │
NDArray[A]        ← N-dimensional strided array (new)
    │
Matrix[A]         ← 2D NDArray with convenience API (migration from existing)
```

Future (separate module):
```
NDArray[A]
    │
Tensor            ← NDArray[Double] + AD metadata (vecxt_ad module)
```

### How Matrix becomes a 2D NDArray

**Not a breaking rewrite.** The plan:

1. `NDArray[A]` is introduced with `Array[A] + shape + strides + offset`
2. `Matrix[A]` keeps its existing API but its internals become `ndim == 2` specialization of the same memory model
3. Migration is gradual — Matrix can delegate to NDArray for shared ops (element access, reshape, transpose) while keeping its own ergonomic API (`@@` for matmul, `row()`, `col()`, etc.)

### How Tensor shares the model (future)

A future `Tensor` in a separate `vecxt_ad` module would be:

```scala
case class Tensor(
  value: NDArray[Double],   // the actual data
  // AD-specific fields:
  grad: Option[NDArray[Double]],
  backprop: NDArray[Double] => Unit
)
```

It doesn't wrap or duplicate NDArray — it *composes* with it. The NDArray is the storage; the Tensor adds gradient semantics. Same memory model, different concerns.

---

## NDArray[A] Detailed Design

### Core type

```scala
class NDArray[A] private[ndarray] (
  val data: Array[A],
  val shape: Array[Int],
  val strides: Array[Int],
  val offset: Int
):
  lazy val ndim: Int = shape.length
  lazy val numel: Int = shape.product
  lazy val isContiguous: Boolean = /* check strides match dense layout */
```

No `@specialized` annotation. `Array[A]` for primitive `A` (Double, Int, etc.) is already
a primitive array at the JVM level (`double[]`, `int[]`) — no boxing occurs on element access.
The operations that matter (SIMD, BLAS, reductions) are written per-type via extension methods
anyway, so specialization on the container class adds complexity without measurable benefit.
Scala 3's `@specialized` is inherited from Scala 2, poorly maintained, and can silently
de-specialize when combined with `inline` methods, opaque types, or extension methods.

### Design decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Element type | Generic `A`, **no `@specialized`** | `Array[A]` for primitives is already unboxed (`double[]`). Ops are per-type via extension methods. `@specialized` adds complexity without benefit in Scala 3. |
| Shape representation | `Array[Int]` | Runtime, heap-efficient, no type-level arithmetic |
| Strides | Explicit `Array[Int]` | Enables views, transpose, broadcast without copying |
| Default layout | Column-major (F-order) for all ranks | Consistent with existing Matrix; BLAS-native; 2D slices of N-D arrays are directly BLAS-compatible; Julia validates this approach |
| Constructor | Private + factory with BoundsCheck | Match existing Matrix pattern |
| Bounds checking | Erasable `BoundsCheck` typeclass | Existing pattern, zero-cost in production |
| API surface | Extension methods | Existing pattern, platform-specific dispatch |

### Layout convention

**Column-major (Fortran-order) for all ranks.** The first index varies fastest in memory.

For a dense array with `shape=[d₀, d₁, ..., dₙ₋₁]`, the default strides are `[1, d₀, d₀·d₁, ...]`.

Rationale:
- Consistent with existing Matrix (column-major, BLAS-native)
- 2D slices of N-D arrays are naturally column-major → directly BLAS-compatible, no layout conversion needed
- Julia validates column-major-throughout as a performant, ergonomic choice for numerical computing
- Row-major is still fully representable via different strides — nothing precludes it
- Users who need row-major can construct with explicit strides or use a `rowMajor` factory

Detection methods (`isRowMajor`, `isColMajor`, `isContiguous`) enable fast-path optimizations regardless of layout.

### Relationship to existing code

| Existing | NDArray equivalent |
|----------|-------------------|
| `Matrix.raw` | `NDArray.data` |
| `Matrix.rows` | `NDArray.shape(0)` |
| `Matrix.cols` | `NDArray.shape(1)` |
| `Matrix.rowStride` | `NDArray.strides(0)` |
| `Matrix.colStride` | `NDArray.strides(1)` |
| `Matrix.offset` | `NDArray.offset` |
| `Matrix.hasSimpleContiguousMemoryLayout` | `NDArray.isContiguous` |
| `Matrix.isDenseColMajor` | `NDArray.isColMajor` (for ndim==2) |
| `Array[Double]` (1D ops) | Can be lifted to `NDArray` with `shape=[n], strides=[1]` |

### Broadcasting rules

**Broadcasting is explicit, not implicit.** Binary ops (`+`, `-`, `*`, `/`) require same-shape
operands — consistent with vecxt's existing `Array[Double]` ops, which throw on length mismatch.

Broadcasting is performed via an explicit `broadcastTo(targetShape)` method that returns a
zero-copy view (stride-0 expansion). The broadcasting mechanics follow NumPy semantics:
1. Shapes are right-aligned
2. Dimensions are compatible if equal or one of them is 1
3. A dimension of 1 is broadcast (stride 0) to match the other

A convenience `NDArray.broadcastPair(a, b)` returns both operands broadcast to their common shape.

This explicit model means broadcasting is a **first-class operation** in any computation graph.
For AD (M7), `broadcastTo` is a distinct node with a clean VJP rule (sum-reduce), rather than
hidden bookkeeping inside every binary op.

---

## Implementation Plan

### Milestone 0: Preparation (no code changes)
- Agree on this design
- alongside Matrix

### Milestone 1: NDArray core type + factories
**Goal:** The type exists, can be constructed, and has basic properties.

- [ ] `NDArray[A]` class with `data`, `shape`, `strides`, `offset` (no `@specialized` — see design decisions)
- [ ] Private constructor + `NDArray.apply(...)` with `BoundsCheck` validation
- [ ] Stride validation (generalization of `strideMatInstantiateCheck`)
- [ ] Factory methods: `NDArray.zeros`, `NDArray.ones`, `NDArray.fill`, `NDArray.fromArray` (1D), `NDArray.fromMatrix`
- [ ] Properties: `ndim`, `numel`, `isContiguous`, `isRowMajor`, `isColMajor`
- [ ] `toString` / `layout` for debugging
- [ ] Cross-platform (shared `src/` only — no platform-specific code yet)
- [ ] Unit tests for construction and property checking

**Deliverable:** `NDArray` can be created and inspected. No operations yet.

### Milestone 2: Indexing + views
**Goal:** You can read/write elements and create views without copying.

- [ ] Single-element access: `ndarray(i, j, k)` → `A` (varargs `Int*` or `Array[Int]`)
- [ ] Single-element update: `ndarray(i, j, k) = value`
- [ ] Slice/view: `ndarray.slice(dim, range)` → new NDArray (adjusted strides/offset, no copy)
- [ ] `transpose` (permute dimensions) → new NDArray (permuted strides, no copy)
- [ ] `reshape` (contiguous arrays only, else error/copy)
- [ ] `squeeze` (remove dimensions of size 1)
- [ ] `unsqueeze` / `expandDims` (add dimension of size 1)
- [ ] `flatten` → 1D NDArray (copy if non-contiguous)
- [ ] `toArray` → contiguous `Array[A]`  (copy if needed)
- [ ] Multi-dimensional `::` slicing: `ndarray(::, 1 until 3, ::)` (see below)
- [ ] Cross-platform tests

**Multi-dim indexing design:** Reuse the existing `RangeExtender = Range | Array[Int] | ::.type` from
Matrix via a varargs `apply(selectors: RangeExtender*)` on `NDArray[A]`. One selector per dimension.
`::` keeps the full extent, `Range` slices, `Array[Int]` gathers. When all selectors are `::` or
contiguous `Range`, the result is a **zero-copy view** (adjust offset/shape, share strides/data).
When any selector is `Array[Int]` or non-unit-step `Range`, a copy is made into a fresh col-major
NDArray. This mirrors Matrix `submatrix` semantics exactly.

The existing per-arity `apply(i0: Int, ...)` element-access overloads are unaffected — `Int` vs
`RangeExtender` resolves unambiguously. Single-`Int` dimension collapsing in the varargs form
(NumPy's `cube[0, :, :]` → 2-D result) is deferred.

See `site/notes/ndarray-multidim-indexing-design.md` for full implementation and verification details.

**Deliverable:** Full indexing and view algebra. This is the foundation that everything else builds on.

### Milestone 3: Element-wise operations (Double)
**Goal:** Arithmetic works for `NDArray[Double]`, with platform-specific acceleration.

- [ ] Binary ops: `+`, `-`, `*`, `/` (element-wise, **same-shape required**)
- [ ] Scalar ops: `ndarray + scalar`, `scalar * ndarray`, etc.
- [ ] Unary ops: `neg`, `abs`, `exp`, `log`, `sqrt`, `tanh`, `sigmoid`
- [ ] In-place variants: `+=`, `-=`, `*=`, `/=`
- [ ] Comparison ops returning `NDArray[Boolean]`: `>`, `<`, `>=`, `<=`, `=:=`, `!:=`
- [ ] Platform-specific implementations:
  - JVM: SIMD `DoubleVector` for contiguous arrays
  - JS/Native: while loops
- [ ] Explicit broadcasting: `broadcastTo(shape)` → zero-copy view; `NDArray.broadcastPair(a, b)`
- [ ] Cross-platform tests with tolerance for floating point

**Deliverable:** NDArray is useful for numeric computation. Broadcasting is explicit and correct.

### Milestone 4: Reduction operations
**Goal:** Aggregation along axes.

- [ ] Full reductions: `sum`, `mean`, `min`, `max`, `product`, `variance`, `norm` (L2)
- [ ] Axis reductions: `sum(axis)`, `mean(axis)`, `min(axis)`, `max(axis)`, `product(axis)` → NDArray with one fewer dimension
- [ ] `argmin`, `argmax` (full and per-axis)
- [ ] `dot` (1D), `matmul`/`@@` (2D) — delegate to existing BLAS for 2D case
- [ ] Platform-specific fast paths for contiguous data
- [ ] Cross-platform tests

**Key design decisions:**

- **Axis parameter is `Int`**, not `DimensionExtender`. The `Dimension` enum (`Rows`, `Cols`) is
  ergonomic for 2-D but doesn't generalise to N-D.
- **Axis reductions remove the collapsed dimension** (NumPy `keepdims=False` default). Users who
  want keepdims can `unsqueeze(axis)` on the result.
- **Full reductions return `Double`**, not 0-D NDArray. Vecxt doesn't use 0-D NDArrays.
- **Col-major fast path delegates to existing `Array[Double]` extensions** (`sumSIMD`, `maxSIMD`,
  `norm` via BLAS `dnrm2`, `dot` via BLAS `ddot`, `argmax`/`argmin`). No new JVM-specific code.
- **`matmul` is 2-D only; `dot` is 1-D only.** No batched matmul. `matmul` delegates to
  `Matrix.@@` (BLAS `dgemm`); non-contiguous inputs are materialised first.
- All code lives in shared `src/ndarrayReductions.scala`; platform acceleration comes for free
  through delegation to already-SIMD-accelerated `Array[Double]` methods.

See `site/notes/m4-reductions-design.md` for full implementation, worked examples, and verification plan.

**Deliverable:** Statistical and aggregation workloads run on NDArray.

### Milestone 5: Matrix ↔ NDArray bridge
**Goal:** Matrix and NDArray interoperate seamlessly.

- [ ] `Matrix.toNDArray` → 2D NDArray (zero-copy, shared backing array)
- [ ] `NDArray.toMatrix` → Matrix (only if ndim==2, zero-copy)
- [ ] `Array[Double].toNDArray` → 1D NDArray
- [ ] Ensure existing Matrix operations still work (regression tests)
- [ ] Consider: should Matrix internally delegate to NDArray for shared operations? (Evaluate performance implications first)
- [ ] Documentation: migration guide for users

**Deliverable:** The two types coexist and convert freely. No breaking changes to Matrix API.

### Milestone 6: Extended element types + polish
**Goal:** NDArray works for Int, Float, Boolean with appropriate ops.

- [ ] `NDArray[Int]`: arithmetic, comparisons, reductions
- [ ] `NDArray[Boolean]`: logical ops (`&`, `|`, `!`), `any`, `all`, `countTrue`
- [ ] `NDArray[Float]`: arithmetic (useful for ML workloads)
- [ ] Boolean indexing: `ndarray(boolNdarray)` → filtered 1D result
- [ ] `where(condition, x, y)` → element-wise conditional
- [ ] Performance benchmarks vs existing `Array[Double]` ops
- [ ] API review and cleanup

**Deliverable:** NDArray is a general-purpose N-dimensional array with full type support.

---

## Milestone dependency graph

```
M0 (preparation)
 └─► M1 (core type)
      └─► M2 (indexing + views)
           ├─► M3 (element-wise ops)
           │    └─► M4 (reductions)
           │         └─► M6 (extended types + polish)
           └─► M5 (Matrix bridge)
```

M3 and M5 can proceed in parallel after M2.

---

## Future milestones (out of scope for NDArray, but planned)

### Milestone 7: AD module (`vecxt_ad`)
- Separate Mill module depending on `vecxt`
- `Tensor` type composing `NDArray[Double]` + gradient tracking
- Forward-mode AD via `Jet[NDArray[Double]]`
- Reverse-mode AD via `Node` with backprop closures
- VJP rules for all NDArray primitives

### Milestone 8: MathAST + interpreters
- Expression tree (`MathAST[A]`)
- `Ops[A]` typeclass with instances for `NDArray`, `Jet[NDArray]`, `Node`
- Plain evaluator, forward AD evaluator, reverse AD evaluator

---

## Open questions

2. ~~**Default layout**~~ → Column-major (F-order) for all ranks (decided). Consistent with existing Matrix and BLAS.

3. **Copy semantics for views:** NDArray views share backing data. Mutation through one view is visible through others. This is the NumPy/PyTorch model and avoids unnecessary copies. Should we support copy-on-write? (Recommendation: no, too complex, just document the aliasing behavior.) Recommendation, copy on write would silently tank performance. It would be easy enough to write an immutable wrapper over the top if someone wants it. Decided. Recommendation accepted, no copy-on-write. Document aliasing semantics clearly.

4. **Naming:** `NDArray[A]` vs `NdArray[A]` vs `Tensor[A]`? Recommendation: `NDArray` for the data structure. Reserve `Tensor` for the AD-aware type in a future module. NDArray - decided.

5. **Int indexing API:** Varargs `apply(indices: Int*)` is convenient but allocates. Alternative: overloads for 1, 2, 3, 4, N cases. Or: `IArray[Int]` to signal no mutation. Recommendation: specific overloads for 1-4D, varargs for N>4. Agreed to follow recommendation.

6. **Broadcasting: implicit or explicit?** Implicit broadcasting (NumPy-style, where `+` silently expands shapes) vs explicit (`broadcastTo` required before binary ops). **Decided: explicit.** Rationale:
   - **Consistent** with existing `Array[Double]` ops which require same length — no implicit expansion anywhere in vecxt.
   - **Simpler binary ops** — `+` is always same-shape; no broadcasting code path, no shape-mismatch error messages.
   - **Cleaner AD** — `broadcastTo` is a first-class node in the computation graph with a clean VJP rule (sum-reduce along broadcast axes). Binary ops have trivial same-shape gradients.
   - **Better errors** — "cannot broadcastTo shape [4,3] from shape [2,3]" is more actionable than "cannot broadcast shapes [4,3] and [2,3] in +".
   - **`broadcastTo` is free** — it's a zero-copy view (stride-0). The verbosity cost is one method call that documents intent.
   - `NDArray.broadcastPair(a, b)` provides a convenience for the common case of broadcasting two operands to their common shape.

---
