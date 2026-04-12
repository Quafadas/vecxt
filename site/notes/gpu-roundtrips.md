# GExpr CPU↔GPU Round-Trip Analysis

## Status Quo

Every `GExpr.run` call walks the AST and dispatches GPU work.
The compiler has two paths:

| Path | When | Transfers |
|------|------|-----------|
| **Single-input** (`runSingleInput`) | All nodes trace back to one `GLeaf` | 1 upload + 1 download |
| **Multi-input** (`materialise`) | Multiple distinct `GLeaf`s in tree | **recursive** — see below |

### Single-input: Good

`a.gpu.exp.sin.+(3.0f).run` fuses into **one** `GFunction` kernel.
One CPU→GPU upload, one GPU→CPU download. No intermediate round-trips.
This is the ideal case.

### Multi-input: Bad

Every `GBinaryOp` node forces both children to **materialise to `Array[Float]` on CPU**,
then re-uploads them for the binary operation. Each `BinaryOp` is a separate `GProgram`.

Unary/scalar nodes wrapping binary sub-trees (`materialiseWrapped`) also force
an additional GPU dispatch: materialise inner → CPU → new GLeaf → upload → GPU → CPU.

## Concrete Example: `(a.exp + b.sin) * 2.0f`

AST:
```
ScalarOp(Mul, 2.0)
  └─ BinaryOp(Add)
       ├─ UnaryOp(Exp)
       │    └─ GLeaf(a)
       └─ UnaryOp(Sin)
            └─ GLeaf(b)
```

Transfer log (from test with `logGExprTransfers = true`, n=3 floats):

```
[GExpr] run: 2 leaf(s), expr=Scalar(Mul, 2.0)
[GExpr]   materialiseWrapped: multi-leaf under Scalar(Mul, 2.0) — forcing inner materialise + re-upload
[GExpr]   materialise BinaryOp(Add) — will materialise both sides to CPU then re-upload
[GExpr]   CPU→GPU upload 3 floats (runSingleInput)          ← a for exp
[GExpr]   GPU→CPU download 3 floats (runSingleInput)        ← exp(a) back to CPU
[GExpr]   CPU→GPU upload 3 floats (runSingleInput)          ← b for sin
[GExpr]   GPU→CPU download 3 floats (runSingleInput)        ← sin(b) back to CPU
[GExpr]   CPU→GPU upload 2×3 floats (runBinary Add)         ← exp(a) + sin(b) re-uploaded
[GExpr]   GPU→CPU download 3 floats (runBinary Add)         ← result back to CPU
[GExpr]   CPU→GPU upload 3 floats (runSingleInput)          ← result re-uploaded for *2.0
[GExpr]   GPU→CPU download 3 floats (runSingleInput)        ← final result
```

**Summary: 4 GPU dispatches, 5 uploads (11 floats up), 4 downloads (12 floats down).**

The ideal: 1 dispatch, 1 upload of `a` and `b` (6 floats), 1 download (3 floats).

## Where Exactly the Round-Trips Happen

| Location | What crosses the bus | Why |
|----------|---------------------|-----|
| `runSingleInput` → `gf.run(leaf.data)` | CPU→GPU (leaf data) + GPU→CPU (result) | `GFunction.run` uploads input, downloads result. Fine for leaf chains. |
| `materialise(BinaryOp)` → recursive `materialise(left)` / `materialise(right)` | Forces each side to `Array[Float]` on CPU | `runBinary` needs `Array[Float]` inputs — that's the problem. |
| `runBinary` → `GBufferRegion.runUnsafe` | CPU→GPU (both operands) + GPU→CPU (result) | The `BinLayout` uploads both sides as `GBuffer`, reads result back. |
| `materialiseWrapped` → wraps in new `GLeaf` | Intermediate GPU→CPU then CPU→GPU | Inner result downloaded to CPU, wrapped as `GLeaf`, re-uploaded in outer op. |

**Root cause**: `materialise` returns `Array[Float]` (CPU memory). Every sub-tree must be
fully evaluated to CPU before it can participate in a parent operation.

## Can We Eliminate `dimCheck` and Stay on GPU?

**No** — `dimCheck` is not the bottleneck, and removing it alone doesn't help.

The issue isn't the dimension check (which is `O(1)` — just comparing two ints).
The issue is the **intermediate representation**: `materialise` returns `Array[Float]`,
which forces every sub-expression to fully round-trip through CPU memory.

Even without `dimCheck`, `runBinary` still takes `Array[Float]` arguments.

## The Fix: Two-Phase Compile

### Phase 1: Shape Analysis (CPU only, no data movement)

Walk the AST and compute dimensions at each node. All our ops are elementwise,
so the rules are trivial:

- `GLeaf(data)` → `data.length`
- `GUnaryOp(input, _)` → same as input
- `GScalarOp(input, _, _)` → same as input
- `GClampOp(input, _, _)` → same as input
- `GBinaryOp(left, right, _)` → assert `dim(left) == dim(right)`, return it

This is `O(nodes)`, touches no GPU, and catches all dimension errors eagerly.

### Phase 2: Compile Entire AST → Single `GProgram` (one upload, one download)

Once shapes are validated, collect all unique `GLeaf` nodes. These become `GBuffer`
entries in a single `Layout`. Then walk the AST and emit Cyfra DSL code that reads
from the appropriate buffer at `GIO.invocationId`.

For `(a.exp + b.sin) * 2.0f`, the compiled program would be conceptually:

```scala
case class FusedLayout(
  in0: GBuffer[Float32],   // a
  in1: GBuffer[Float32],   // b
  out: GBuffer[Float32]
) derives Layout

val program = GProgram.static[GFunctionParams, FusedLayout](...): layout =>
  val idx = GIO.invocationId
  val v0 = F.exp(layout.in0.read(idx))    // exp(a[i])
  val v1 = F.sin(layout.in1.read(idx))    // sin(b[i])
  val v2 = (v0 + v1) * 2.0f              // (exp(a[i]) + sin(b[i])) * 2
  for _ <- layout.out.write(idx, v2)
  yield GStruct.Empty()
```

**Result: 1 GPU dispatch, 2 uploads (a, b), 1 download (result). Zero intermediate round-trips.**

For deeper trees with many binary ops like `((a + b) * (c - d)).exp`,
the same approach works — all 4 leaves become buffers in one layout,
one kernel computes the full expression per element.

### Cyfra's `GExecution` for Multi-Stage Pipelines

If we ever need non-elementwise operations (reductions, scans, etc.) that require
multiple kernel stages, Cyfra's `GExecution` composes `GProgram`s into a pipeline
where intermediate buffers **stay on GPU** — no CPU round-trip between stages.
This is documented in `gvecxt/cyfra docs/gpu-pipelines.md`.

## Implementation Sketch

```
Phase 1                              Phase 2
───────                              ───────
walk AST                             collect distinct GLeaf → index map
  → compute dim(node) for each       generate case class with N GBuffers + 1 out
  → validate BinaryOp dims match     walk AST, emit Cyfra DSL per node
  → early error if mismatch          GProgram.static[Params, Layout](...)
  (zero GPU work)                    GBufferRegion.allocate → runUnsafe
                                       upload leaves once, download result once
```

### Challenge: Dynamic Layout

Cyfra's `Layout` is derived at compile time via a `derives Layout` macro.
For a fixed number of inputs (1, 2, 3, …) we'd need pre-defined layout case classes:

```scala
case class Layout1(in0: GBuffer[Float32], out: GBuffer[Float32]) derives Layout
case class Layout2(in0: GBuffer[Float32], in1: GBuffer[Float32], out: GBuffer[Float32]) derives Layout
case class Layout3(...) derives Layout
// etc.
```

Alternatively, since all our ops are elementwise on `Float32`, we might be able to
pack multiple inputs into one `GBuffer` (interleaved or offset regions) and use
index arithmetic to address them. This would allow a single Layout definition to
handle any number of inputs. Worth investigating.
