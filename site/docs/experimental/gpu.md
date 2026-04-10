# GPU Experiment Results

The `gvecxt` module explores GPU-accelerated elementwise computation via [Cyfra](https://github.com/ComputeNode/cyfra), a Scala DSL that compiles to SPIR-V / Vulkan compute shaders.

This page summarises the findings from the experiment and maps them to the POCs outlined in the [sparta feasibility analysis](../../notes/sparta.md).

## What Was Built

### Expression Tree (`GExpr` / `GNDExpr`)

A lazy, backend-agnostic AST for elementwise float operations:

```scala
val a = GNDArray.matrix(dataA, 1000, 1000)
val b = GNDArray.matrix(dataB, 1000, 1000)
val result = ((a + b) * 2.0f).exp  // builds AST — no work yet
result.run       // GPU dispatch via Cyfra
result.runCpu    // fused CPU interpreter (single pass, no intermediates)
```

Supported AST nodes: unary (`neg`, `abs`, `exp`, `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `log`), scalar (`+`, `-`, `*`, `/`, `pow`, `clamp`), elementwise binary (`+`, `-`, `*`, `/`), reshape, broadcast (shape analysis only), matmul (shape analysis only), transpose (shape analysis only).

### Phase 1: Shape Analysis

Walks the AST on the CPU and validates dimensions at every node. No GPU work, no data movement. Catches mismatches eagerly before any allocation.

### Phase 2: Fused Single-Dispatch Compiler

All leaf arrays are packed into one contiguous `GBuffer`. A single `GProgram` kernel fuses the entire expression tree — each GPU thread evaluates the full pipeline for one element using index arithmetic to read from each leaf's region of the packed buffer.

For `(a.exp + b.sin) * 2.0f`:
- **Before (Phase 0):** 4 GPU dispatches, 5 uploads, 4 downloads
- **After (Phase 2):** 1 GPU dispatch, 1 upload, 1 download

### Fused CPU Interpreter (`runCpu`)

The same `GNDExpr` tree can be evaluated on the CPU via a per-element recursive interpreter — single pass, no intermediate `Array[Float]` allocations. This isolates fusion effects from hardware effects in benchmarking.

## Benchmark Results

Three backends compared: **GPU** (Cyfra/Vulkan), **CPU unfused** (vecxt `NDArray[Float]` ops — vectorised via Java Vector API but separate loop + allocation per op), **CPU fused** (recursive AST interpreter — single pass but scalar).

**Light pipeline:** `(a + b) * 2.0 |> exp` — ~4 FLOPs/element

| Size | GPU | CPU unfused | CPU fused |
|------|-----|-------------|-----------|
| 1M | 28ms | 55ms | **27ms** |
| 10M | 156ms | **123ms** | 245ms |

**Heavy pipeline:** `sin(exp(a+b)) * cos(a*b) + atan(a/b) |> exp |> sqrt |> log` — ~15 FLOPs/element

| Size | GPU | CPU unfused | CPU fused |
|------|-----|-------------|-----------|
| 1M | **13ms** | 174ms | 158ms |
| 10M | **96ms** | 747ms | 1596ms |
| 100M | **2.1s** | 8.9s | 16.2s |

### Analysis

1. **GPU dominates compute-heavy pipelines** — 12× faster at 1M, 4× at 100M for the heavy pipeline. The advantage narrows at scale because transfer cost (uploading 200M floats + downloading 100M) grows linearly while compute grows sub-linearly on a fixed GPU.

2. **Kernel fusion matters more than hardware.** The CPU unfused path allocates ~12 temporary arrays for the heavy pipeline (4.8 GB at 100M elements). The GPU's real advantage isn't raw FLOP throughput — it's that the fused kernel eliminates all intermediate allocations and does a single pass over memory.

3. **The fused CPU interpreter is slow at scale** because it does a recursive AST walk per element (~15 virtual dispatches per float). The JVM can't SIMD-vectorise a recursive tree walk, so it loses to the unfused path's tight vectorised loops (Java Vector API) despite avoiding intermediate allocations. The optimal CPU path would be a flat bytecode interpreter or JIT-compiled fused loop.

4. **Transfer overhead is the GPU bottleneck.** The light pipeline (4 FLOPs/element) loses to CPU at 10M because the arithmetic intensity (FLOPs per byte transferred) is too low. GPU wins when there's enough compute to amortise the bus cost.

## Mapping to Sparta POCs

| Sparta POC | Status | Notes |
|------------|--------|-------|
| **POC 4: Cyfra Smoke Test** | ✅ Satisfied | Round-trip works: CPU → GPU buffer → compute → CPU. Fused dispatch for arbitrary expression trees with multiple inputs. Acceptable latency for arrays ≥ 1M elements. |
| **POC 5: MathExpr → Cyfra Pipeline** | ✅ Partially satisfied | The `GNDExpr` AST → Cyfra compiler is exactly this pattern. It walks an expression tree and emits Cyfra DSL calls that build a single SPIR-V pipeline. The "AST-to-pipeline compilation" approach from sparta works and is implemented. |
| **POC 1: MathTrig[NDArray]** | ⬜ Not addressed | The `GNDExpr` tree is a separate IR from mathlify's `MathExpr`. Bridging them requires implementing `MathTrig[GNDExpr]` instances. The `zero`/`one`/`fromDouble` shape problem identified in sparta remains unresolved. |
| **POC 2/3: AD (forward/reverse)** | ⬜ Not addressed | No differentiation work done. But the fused GPU pipeline is the execution substrate that AD would target. |

### Key Sparta Risk Assessment Updates

| Risk | Sparta Rating | Updated Rating | Evidence |
|------|---------------|----------------|----------|
| Cyfra op coverage / maturity | 🔴 High | 🟡 Medium | Elementwise ops are solid. Reductions, matmul, comparisons missing from AST but Cyfra DSL has the primitives. |
| Cyfra compile-time DSL vs runtime AST mismatch | 🔴 High | 🟢 Resolved | The packed-buffer `GProgram.static` approach successfully bridges runtime AST structure to compile-time Layout. Single `FusedLayout` handles any number of inputs. |
| Per-op GPU dispatch overhead | — | 🟢 Resolved | Fused dispatch eliminates this entirely. One kernel for the full expression tree. |

## Missing GPU Kernel Operations for NDArray Parity

The `GNDExpr` AST has shape analysis for broadcast, matmul, and transpose, but lowering to GPU kernels throws `UnsupportedOperationException`. Below is a complete gap analysis.

### Operations with AST Nodes but No Lowering

| Operation | Shape Analysis | GPU Lowering | What's Needed |
|-----------|---------------|--------------|---------------|
| **Broadcast** | ✅ Full validation | ❌ Throws | Cyfra kernel with per-dimension stride logic (stride=0 for broadcast dims). Conceptually simple but requires non-trivial index arithmetic. |
| **MatMul** (`@@`) | ✅ Full shape rules (batched, 1D, 2D) | ❌ Throws | Tiled matmul kernel via `GProgram`. Cyfra has the primitives (`GBuffer.read`, shared memory via `GSeq`) but no built-in GEMM. This is the hardest single operation to implement efficiently. |
| **Transpose** (`.T`) | ✅ Shape reversal | ❌ Throws | For the fused elementwise path: just swap stride order in index arithmetic. For materialised output: needs a copy kernel with stride-aware indexing. |

### Operations Completely Absent from the AST

| Category | Operations | Cyfra Support | Effort |
|----------|-----------|---------------|--------|
| **Reductions** | `sum`, `max`, `min`, `mean`, `argmax`, `argmin` (global + per-axis) | `GSeq.fold` / multi-stage `GExecution` pipeline | Medium-High. Parallel reduction requires multi-stage kernels (local reduce → global reduce). Cyfra's `GExecution` pipeline keeps intermediates on-GPU between stages. |
| **Comparisons** | `>`, `<`, `>=`, `<=`, `===`, `!==` → boolean array | Cyfra has `GBoolean` + comparison ops | Low. Add AST nodes + lowering — Cyfra directly supports this. |
| **Conditional/Select** | `where(cond, x, y)` | Cyfra has `when(...).otherwise(...)` | Low. Straightforward to add alongside comparisons. |
| **Scatter/Gather** | `arr[indices]`, boolean masking | `GBuffer.read(idx)` / `.write(idx, val)` | Medium. Gather is easy (index indirection). Scatter has race conditions requiring atomics. |
| **Elementwise binary min/max** | `min(a, b)`, `max(a, b)` | `F.min`, `F.max` available | Trivial. Just add a `BinaryFn` enum variant. |
| **Additional math** | `atan2`, `mod`, `mix`/lerp, `smoothstep` | All available in Cyfra | Low. Add enum variants and lowering cases. |
| **Type support** | `Int32`, `Float64`, `Boolean` arrays | Cyfra supports all | Medium. Current AST is `Float`-only. Would need type-parameterisation or separate ASTs. |

### Priority Order for Implementation

1. **Comparisons + conditional select** — unblocks boolean masking, `where`, and activation functions like ReLU (`where(x > 0, x, 0)`)
2. **Broadcast lowering** — required for any real neural network (bias addition, scalar operations on batched data)
3. **Reductions** — `sum` is critical for loss functions, `mean` for normalisation
4. **Transpose lowering** — needed for matmul VJP rules (`∂L/∂A = adj @@ Bᵀ`)
5. **MatMul** — the big one. Essential for any practical neural network but also the most complex GPU kernel to write correctly and performantly
6. **Scatter/gather** — needed for embedding layers, advanced indexing

### What Cyfra Itself Would Need

Cyfra's DSL already provides the building blocks for everything above. The gaps are in the `GExpr`/`GNDExpr` AST and lowering code, not in Cyfra's capabilities. The one area where Cyfra might need upstream work is **shared memory / workgroup-level operations** for efficient tiled matmul and parallel reductions — this depends on how much of Vulkan's compute model `GProgram` currently exposes.
