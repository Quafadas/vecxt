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
class NDArray[@specialized(Double, Int, Float, Boolean) A] private[ndarray] (
  val data: Array[A],
  val shape: Array[Int],
  val strides: Array[Int],
  val offset: Int
):
  lazy val ndim: Int = shape.length
  lazy val numel: Int = shape.product
  lazy val isContiguous: Boolean = /* check strides match dense layout */
```

### Design decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Element type | Generic `A` with `@specialized` | Match existing Matrix pattern; avoid Double-only lock-in |
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

Follow NumPy broadcasting semantics:
1. Shapes are right-aligned
2. Dimensions are compatible if equal or one of them is 1
3. A dimension of 1 is broadcast (stride 0) to match the other

This is critical for AD (gradient accumulation involves broadcast reduction).

---

## Implementation Plan

### Milestone 0: Preparation (no code changes)
- Agree on this design
- alongside Matrix

### Milestone 1: NDArray core type + factories
**Goal:** The type exists, can be constructed, and has basic properties.

- [ ] `NDArray[A]` class with `data`, `shape`, `strides`, `offset`
- [ ] `@specialized(Double, Int, Float, Long, Boolean)`
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
- [ ] Cross-platform tests

**Deliverable:** Full indexing and view algebra. This is the foundation that everything else builds on.

### Milestone 3: Element-wise operations (Double)
**Goal:** Arithmetic works for `NDArray[Double]`, with platform-specific acceleration.

- [ ] Binary ops: `+`, `-`, `*`, `/` (element-wise, with broadcasting)
- [ ] Scalar ops: `ndarray + scalar`, `scalar * ndarray`, etc.
- [ ] Unary ops: `neg`, `abs`, `exp`, `log`, `sqrt`, `tanh`, `sigmoid`
- [ ] In-place variants: `+=`, `-=`, `*=`, `/=`
- [ ] Comparison ops returning `NDArray[Boolean]`: `>`, `<`, `>=`, `<=`, `==`
- [ ] Platform-specific implementations:
  - JVM: SIMD `DoubleVector` for contiguous arrays
  - JS/Native: while loops
- [ ] Broadcasting implementation (shape compatibility check + stride-0 expansion)
- [ ] Cross-platform tests with tolerance for floating point

**Deliverable:** NDArray is useful for numeric computation. Broadcasting works.

### Milestone 4: Reduction operations
**Goal:** Aggregation along axes.

- [ ] Full reductions: `sum`, `mean`, `min`, `max`, `variance`
- [ ] Axis reductions: `sum(axis=k)`, `mean(axis=k)`, etc. → NDArray with one fewer dimension
- [ ] `argmin`, `argmax` (full and per-axis)
- [ ] `dot` (1D), `matmul` (2D) — delegate to existing BLAS for 2D case
- [ ] `norm` (L1, L2, Linf)
- [ ] Platform-specific fast paths for contiguous data
- [ ] Cross-platform tests

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

1. ~~**Module placement**~~ → `vecxt/src/` (decided)

2. ~~**Default layout**~~ → Column-major (F-order) for all ranks (decided). Consistent with existing Matrix and BLAS.

3. **Copy semantics for views:** NDArray views share backing data. Mutation through one view is visible through others. This is the NumPy/PyTorch model and avoids unnecessary copies. Should we support copy-on-write? (Recommendation: no, too complex, just document the aliasing behavior.)

4. **Naming:** `NDArray[A]` vs `NdArray[A]` vs `Tensor[A]`? Recommendation: `NDArray` for the data structure. Reserve `Tensor` for the AD-aware type in a future module.

5. **Int indexing API:** Varargs `apply(indices: Int*)` is convenient but allocates. Alternative: overloads for 1, 2, 3, 4, N cases. Or: `IArray[Int]` to signal no mutation. Recommendation: specific overloads for 1-4D, varargs for N>4.
