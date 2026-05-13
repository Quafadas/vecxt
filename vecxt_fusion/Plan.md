# vecxt_fusion — Kernel Fusion AST Plan

## Goal

Add a kernel-fusion path for `NDArray` operations. The strategy is to introduce a
**typed semantic tensor IR** (`TensorExpr`) between the existing presentation AST
(`MathExpr`) and a future low-level execution IR (`KernelIR`). Fusion planning,
and later AD, both branch from the typed semantic core.

```text
MathExpr                presentation / source AST
  keeps notation: Fraction, Superscript, BracketGroup, ExprSeq, Color, etc.
      ↓ normalize / desugar / lower
TensorExpr (DAG)        typed semantic IR
  scalar ops, tensors, shapes, axes, indexing, reductions — pure, explicit
      ↓ fusion planning / scheduling
KernelIR                execution IR
  loops, buffers, load/store, layout, temporaries
```

`TensorExpr` is implemented first. It is the layer that makes the existing
`MathExpr` actually useful, and it is the layer that fusion and AD will share.

---

## Design Decisions

### 1. DType lives as a runtime field, not in the Scala type system

The IR stores dtype as a value-level field on a small `TType` record (carried by
every node). A typed user-facing façade `Expr[A]` provides compile-time dtype
checking at *construction time*; the underlying IR is monomorphic data.

Rationale:

- Every serious fusion IR (XLA HLO, TVM Relay, MLIR `tensor<...xf32>`) carries
  dtype as a value-level field. ASTs are traversed uniformly by many passes
  (typecheck, normalize, fuse, schedule, layout, codegen) and a parameterised
  `TensorExpr[A]` forces existentials (`TensorExpr[?]`) everywhere.
- Mixed-dtype ops are unavoidable: `Lt: F64 → Bool`, `Cast`, `ArgMax: F64 → I64`,
  `Where(cond: Bool, x: A, y: A)`. A GADT-style IR loses type refinement at
  every traversal.
- Graph/DAG nodes are heterogeneous by definition (`Vector[Node]` cannot be
  `Vector[Node[A]]` for a single `A`).
- Static dtype is captured at the boundary (`lift(nd: NDArray[Double])` →
  tagged `F64`). No information is lost; the IR just stops *re-asserting* it.
- A thin typed builder layer over the IR (`Expr[A]`) recovers compile-time
  safety where users actually program. Same pattern as `frameless` over Spark.

### 2. Shape is an algebra of `Dim`, not an `NDArray[Int]`

Shape is metadata. It is never indexed, strided, laid out, or fused over.
Reusing `NDArray[Int]` for shapes drags in `strides`, `offset`, `layout` and
`BoundsCheck` for what is morally a tiny `Array[Dim]` plus an algebra.

Shape must support symbolic dims for parameters with unknown batch sizes,
broadcasts, and reductions:

```scala
sealed trait Dim
object Dim:
  case class Known(n: Int)     extends Dim   // concrete, e.g. 32
  case class Sym(name: String) extends Dim   // symbolic, e.g. "batch"
  case object Unknown          extends Dim   // erased / runtime
```

Shape unification (broadcast, reduction, matmul) is an algebra on `Dim`, not on
tensors.

Static shape *also* does not belong inside the IR. It can optionally appear in
the typed façade (`Expr[A]` with phantom shape) for users who want it, but the
IR stays uniform.

### 3. The IR is a DAG from day one

Trees are pleasant to build, but every non-trivial fused graph has shared
subexpressions. Encoding sharing as a tree forces a tree→DAG rewrite later.
Canonical form is `TensorGraph(nodes: Vector[Node], output: NodeId)`. A
tree/`Let` view can be reconstructed for pretty-printing.

### 4. Primitive ops are closed enums, not stringly typed

No `FunctionCall("sin", ...)` at the IR level. Every op has known arity, dtype
rules, shape rules, and purity. This is the contract that fusion (and later AD)
relies on.

### 5. The IR is pure, total, and explicit

- No effects (RNG, IO, in-place writes). Effects belong above the IR or below
  it (`KernelIR`).
- Broadcasting is **explicit**: every elementwise binary node has operands of
  identical shape. A `BCast` node is inserted by typechecking.
- Casts are **explicit**: no implicit dtype promotion. A `Cast` node is
  required.
- Binders (`Sum`, `Integral`, indexed expressions) from `MathExpr` are *not*
  lowered into `TensorExpr` until an explicit index/axis model exists. They
  are parse errors at the lowering boundary initially.

### 6. `MapExpr` is not a separate node

Elementwise ops on tensors are just `Binary(Add, t1, t2)` / `Unary(Sin, t)`
with tensor-typed args. "Fusion" is the act of recognising chains of those.
A `Lambda` node is only introduced if/when higher-order ops (e.g. user-defined
scalar kernels) are actually needed.

---

## Core Types (Sketch)

```scala
enum DType:
  case F64, F32, I64, I32, I8, Bool

sealed trait Dim
object Dim:
  case class Known(n: Int)     extends Dim
  case class Sym(name: String) extends Dim
  case object Unknown          extends Dim

final case class Shape(dims: Array[Dim]):
  def rank: Int = dims.length
  def isScalar: Boolean = dims.length == 0

final case class TType(dtype: DType, shape: Shape)

enum UnaryOp:
  case Neg, Sin, Cos, Tan, Exp, Log, Sqrt, Abs, Not, Reciprocal

enum BinaryOp:
  case Add, Sub, Mul, Div, Pow, Min, Max
  case Eq, Neq, Lt, Lte, Gt, Gte
  case And, Or

enum ReduceOp:
  case Sum, Product, Min, Max, All, Any, ArgMax, ArgMin

final case class NodeId(i: Int) extends AnyVal

sealed trait TensorExpr { def tpe: TType }
object TensorExpr:
  case class Const (value: Any,                                  tpe: TType) extends TensorExpr
  case class Param (name: String,                                tpe: TType) extends TensorExpr
  case class Lift  (handle: NDArrayHandle,                       tpe: TType) extends TensorExpr
  case class Unary (op: UnaryOp,  a: NodeId,                     tpe: TType) extends TensorExpr
  case class Binary(op: BinaryOp, a: NodeId, b: NodeId,          tpe: TType) extends TensorExpr
  case class Cast  (to: DType,    a: NodeId,                     tpe: TType) extends TensorExpr
  case class BCast (a: NodeId,    to: Shape,                     tpe: TType) extends TensorExpr
  case class Reduce(op: ReduceOp, a: NodeId, axes: Array[Int],   tpe: TType) extends TensorExpr
  case class Where (c: NodeId,    x: NodeId, y: NodeId,          tpe: TType) extends TensorExpr

final case class TensorGraph(nodes: Vector[TensorExpr], output: NodeId)
```

A typed builder façade sits on top:

```scala
opaque type Expr[A] = NodeId               // plus a builder context
extension [A](e: Expr[A])
  def +(o: Expr[A])(using Numeric[A]): Expr[A]
  def <(o: Expr[A]):                   Expr[Boolean]
  // ...
```

`Expr[A]` is what user code builds against. It compiles down to a `TensorGraph`.

---

# Implementation Plan

The plan is broken into phases that can each be merged and tested
independently. Every phase ends with green tests on JVM, JS and Native unless
the phase is explicitly platform-scoped.

## Phase 0 — Module scaffolding

**Deliverables**

- `vecxt_fusion` module compiles on JVM, JS and Native (it already does, with
  only `MathExpr` in it).
- Add `vecxt_fusion/test/src/` with a single trivial test to wire up `munit`
  cross-platform.
- Decide and document directory layout:
  - `src/` — cross-platform IR, lowering, fusion planner (pure data + algorithms).
  - `src-jvm/`, `src-js/`, `src-native/` — only if/when execution backends land.
- Add `vecxt_fusion/Plan.md` (this file) and a short `Readme.md` linking to it.

**Definition of done:** `./mill vecxt_fusion.__.test` is green on all three
platforms.

## Phase 1 — Core IR data types

**Deliverables**

- `DType`, `Dim`, `Shape`, `TType` in `vecxt.fusion.types`.
- `UnaryOp`, `BinaryOp`, `ReduceOp` enums in `vecxt.fusion.ops`.
- `TensorExpr` ADT and `TensorGraph(nodes, output)` in `vecxt.fusion.ir`.
- `NodeId` as `AnyVal`.
- Structural equality, hashing, and `toString` that prints a readable DAG.

**Non-goals**

- No lowering, no typechecker, no fusion planner yet.

**Tests**

- Build a small graph by hand: `add = x + y`, `out = sin(add)`. Assert node
  count, output id, and round-tripped `toString`.
- Property: every node's `tpe.shape.rank` ≥ 0; `axes` on `Reduce` are within
  rank.

## Phase 2 — Graph builder + typed `Expr[A]` façade

**Deliverables**

- `GraphBuilder` (mutable, lexical scope): hash-consing of nodes so that
  structurally identical subexpressions share `NodeId`.
- `Expr[A]` opaque type over `NodeId` parameterised by a `DTypeOf[A]` type
  class:
  - `given DTypeOf[Double]  = F64`
  - `given DTypeOf[Float]   = F32`
  - `given DTypeOf[Int]     = I32`
  - `given DTypeOf[Boolean] = Bool`
- Operator overloads on `Expr[A]`:
  - `+ - * / unary_-` requiring `Numeric`-style evidence
  - `< <= > >= === =!=` producing `Expr[Boolean]`
  - `&&`, `||`, `!` on `Expr[Boolean]`
  - `.sin`, `.cos`, `.exp`, `.log`, `.sqrt`, `.abs`, `.reciprocal`
- `Expr.param[A](name, shape)`, `Expr.const[A](value)`,
  `Expr.lift(nd: NDArray[A])` entry points.
- `Builder.build(out: Expr[A]): TensorGraph` finaliser.

**Hash-consing rule:** two nodes are equal iff `(constructor, child NodeIds, op,
tpe)` are equal. Floating-point `Const`s use exact bit equality.

**Tests**

- `x + x` produces a graph with one `Param` and one `Binary` (sharing works).
- `(x + y) * (x + y)` produces one shared `Add` node.
- Type-level tests: `Expr[Double] + Expr[Int]` does not compile;
  `Expr[Double] < Expr[Double]: Expr[Boolean]` does.

## Phase 3 — Shape + dtype inference / checking

**Deliverables**

- `TypeCheck.infer(graph): Either[TypeError, TensorGraph]`.
- Rules:
  - Scalar–scalar arithmetic: shapes must be equal (both scalar).
  - Tensor–tensor arithmetic: identical shapes required *after* explicit
    `BCast`. Implicit broadcasting at the user surface inserts a `BCast` node.
  - Comparisons: shape-preserving, dtype → `Bool`.
  - `Cast`: shape-preserving, dtype = target.
  - `Reduce`: removes the listed axes; dtype rules per op
    (`Sum/Product/Min/Max`: input dtype; `All/Any`: Bool; `ArgMax/ArgMin`: I64).
  - `Where`: condition must be `Bool`, branches identical shape + dtype.
- `Shape.broadcast(a, b): Either[ShapeError, Shape]` implementing NumPy-style
  rules over `Dim` (with symbolic dims unified by name).
- `TypeError` ADT with informative messages including offending `NodeId`s.

**Where this lives:** the builder façade inserts `BCast` and `Cast` nodes
during construction, so the resulting graph is always fully explicit. The
standalone `TypeCheck` pass exists for safety/sanity and for graphs constructed
programmatically.

**Tests**

- Golden: `x: f64[3,4] + scalar` → `BCast(scalar, [3,4])` inserted.
- Golden: `sum(x: f64[3,4], axes=[0])` → `f64[4]`.
- Negative: shape mismatch produces a `ShapeError` pointing at the binary node.
- Negative: `Cast` to `Bool` from a non-comparison input is allowed; from
  `Bool` to `F64` is allowed; round-trip preserved.

## Phase 4 — `MathExpr` → `TensorExpr` lowering

**Deliverables**

- `Lower.lower(expr: MathExpr[Double], env: TypeEnv): Either[LowerError, TensorGraph]`.
- Mappings:
  - `Number(v)` → `Const`
  - `Symbol(n)` → `Param` (looked up in `TypeEnv`)
  - `Add/Sub/Mul/Div/Pow` → `Binary`
  - `Neg` → `Unary(Neg, ...)`
  - `FunctionCall("sin"|"cos"|"exp"|"log"|"sqrt"|"abs", [x])` → `Unary(...)`
  - `Fraction(a, b)` → `Binary(Div, a, b)`
  - `Superscript(a, b)` → `Binary(Pow, a, b)` *iff* `b` is a `Number`/`Const`;
    otherwise `LowerError.NonSemanticSuperscript`.
  - `Group`, `BracketGroup`, `Color`, `Style`, `TextNode`, `Operator`,
    `ExprSeq` → recurse into content or error if no content.
  - `Sum`, `Integral`, `Subscript`, `Over`, `Under`, `SubSup` →
    `LowerError.UnsupportedBinder` for now.

**Tests** (AST-shape golden tests, not numeric)

- `x + y` round-trips.
- `Fraction(x, y)` lowers to `Binary(Div, x, y)`.
- `Color("red", Add(x, y))` lowers to `Binary(Add, x, y)`.
- `Superscript(x, 2)` lowers to `Binary(Pow, x, Const(2))`.
- `FunctionCall("sin", [x])` lowers to `Unary(Sin, x)`.
- `Sum(...)` produces `LowerError.UnsupportedBinder`.

## Phase 5 — Normalisation passes

**Deliverables**

- `Normalize.run(graph): TensorGraph`:
  - Constant folding (numeric `Const`s on `Unary`/`Binary` collapsed).
  - Identity removal: `x + 0`, `x * 1`, `x - 0`, `x / 1`, `pow(x, 1)`, etc.
  - Annihilator removal: `x * 0`, `0 / x` (with care over NaN semantics — leave
    `0 / 0` as-is and document).
  - Canonical operand ordering for commutative ops (so hash-cons collapses
    `x + y` and `y + x`).
  - Dead-node pruning (anything not reachable from `output`).
- Pass is idempotent: `normalize(normalize(g)) == normalize(g)` (property test).

**Out of scope here:** algebraic rewrites that change numerics
(`a / b → a * (1/b)`, `x - y → x + (-y)`). These belong in a separate
`Rewrite` module so we can A/B them.

**Tests**

- Idempotence (property).
- Each rule has at least one positive and one negative golden.
- Hash-consing collapses commutative duplicates after normalisation.

## Phase 6 — Fusion planner

**Deliverables**

- `FusionPlanner.plan(graph): FusionPlan`.
- `FusionGroup(inputs: Vector[NodeId], body: TensorGraph, output: NodeId)`.
- `FusionPlan(groups: Vector[FusionGroup], outputAssignment: NodeId → GroupId)`.
- Initial fusion rules (conservative):
  - Fuse chains of elementwise `Unary`/`Binary`/`Where`/`Cast`/`BCast` that
    share input shapes.
  - Fuse a terminal `Reduce` onto its elementwise producer (one terminal
    reduction per group).
  - Do **not** fuse across shape-changing ops other than the terminal reduce.
  - Do **not** fuse nodes with multiple consumers if doing so would duplicate
    significant compute (configurable threshold; default: don't duplicate
    `Reduce` or expensive `Unary`s such as `Exp`/`Log`).
- Cost model: a stub returning estimated FLOPs and bytes-read for a group.

**Tests**

- `sin(x) + cos(x)` → one fusion group with two inputs and shared `x`.
- `sum(sin(x) + 1)` → one group with an embedded terminal reduction.
- `y = sin(x); z = exp(y); w = y + z` → `y` is not duplicated; two groups or
  one group with `y` materialised, depending on cost model.

## Phase 7 — Reference interpreter (correctness oracle)

**Deliverables**

- `Interpreter.eval(graph, env: Map[String, NDArray[?]]): NDArray[?]` built on
  the existing `vecxt` ops (`+`, `*`, `sin`, reductions, broadcast helpers).
- Used **only** for testing and as a correctness oracle. Not on any hot path.

**Tests**

- For each golden lowered from `MathExpr`, evaluate via the interpreter and
  compare to the equivalent direct `vecxt` expression. Float tolerance: 1e-12
  for F64, 1e-5 for F32.
- Same comparison after `Normalize.run` (rewrites must preserve semantics).
- Same comparison after fusion (planner must preserve semantics — groups are
  just scheduling annotations at this stage).

## Phase 8 — KernelIR + first JVM backend (deferred)

This phase is deliberately scoped after the IR + planner are stable. It is
listed here for completeness so design choices in earlier phases stay
compatible with it.

**Deliverables**

- `KernelIR`: `For`, `Allocate`, `Load`, `Store`, `ScalarUnary`, `ScalarBinary`,
  reduction accumulators. Buffers, layouts, indexing.
- `Schedule.lower(plan): KernelIR` — produces one kernel per `FusionGroup`.
- JVM backend: generate a `(inputs) => output` function per group using
  - first cut: straightforward while-loop closures over `Array[Double]` views;
  - second cut: SIMD `jdk.incubator.vector` for elementwise groups whose dtype
    and shape allow it.
- Wire fused kernels behind a `NDArray` extension method, e.g. `fuse { … }`,
  so users opt in.

**Tests**

- Cross-platform numerical parity vs the reference interpreter.
- JVM-only benchmark module entry comparing fused vs unfused for a handful of
  representative expressions.

## Phase 9 — JS / Native backends (deferred)

- `KernelIR` → while-loop Scala code on JS/Native (no SIMD).
- Same cross-platform parity tests as Phase 8.

## Phase 10 — Hooks for AD (deferred, design-only here)

The semantic IR is the layer where reverse-mode AD will live. Recorded now so
nothing in earlier phases blocks it:

- Each `UnaryOp`/`BinaryOp`/`ReduceOp` has (or will gain) a local derivative
  rule expressed as another `TensorGraph` fragment.
- AD is a graph-to-graph transformation producing a backward graph that is
  *also* fused by the same `FusionPlanner`. No special-case backend.

---

## Open Questions (track, don't block)

- **Indexing / gather / scatter.** Needed for embeddings, advanced indexing,
  and any model that isn't pure elementwise + reductions. Plan to add a
  `Gather(src, indices, axis)` node once the elementwise + reduce path is
  solid; do not retrofit binders from `MathExpr.Sum` into this.
- **Matmul / contraction.** Likely a dedicated `Contract(a, b, axes)` node
  rather than expressing matmul as `sum(broadcast(a) * broadcast(b))`. The
  latter is mathematically clean but ruinous for fusion + scheduling.
- **Mixed precision.** Out of scope until we have a single-precision backend.
- **Symbolic shapes (`Dim.Sym`).** API is present from Phase 1, but full
  unification support is only needed when parameters with unknown shapes
  appear; punt the harder unification cases until a concrete use forces them.
- **In-place / aliasing.** The IR is value-semantic. In-place optimisation is
  a `KernelIR`-level concern (buffer reuse), not an IR concern.

---

## Testing Philosophy

The number-one goal of vecxt is **correctness**, and this module inherits that.

- AST-shape golden tests for every lowering and rewrite (cheap, deterministic).
- Property tests for: typechecker totality, normalisation idempotence, fusion
  preserves semantics (via reference interpreter), hash-consing.
- Numerical parity tests via the reference interpreter once Phase 7 lands.
- Cross-platform consistency tests run on JVM, JS and Native.

Performance work happens **only after** Phase 7 establishes a correctness
oracle. No fused kernel ships without a parity test against the interpreter.
