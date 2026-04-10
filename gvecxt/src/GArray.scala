package vecxt.gpu

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion}
import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.library.Functions as F
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.foton.GFunction

// ── AST node types ─────────────────────────────────────────

enum UnaryFn:
  case Neg, Abs, Exp, Sqrt, Sin, Cos, Tan, Asin, Acos, Atan, Log

enum ScalarFn:
  case Add, Sub, Mul, Div, Pow

enum BinaryFn:
  case Add, Sub, Mul, Div

// ── Expression tree ────────────────────────────────────────

sealed trait GExpr:

  // ── Unary elementwise (builds AST, no GPU work) ────────
  def neg: GExpr = GUnaryOp(this, UnaryFn.Neg)
  def abs: GExpr = GUnaryOp(this, UnaryFn.Abs)
  def exp: GExpr = GUnaryOp(this, UnaryFn.Exp)
  def sqrt: GExpr = GUnaryOp(this, UnaryFn.Sqrt)
  def sin: GExpr = GUnaryOp(this, UnaryFn.Sin)
  def cos: GExpr = GUnaryOp(this, UnaryFn.Cos)
  def tan: GExpr = GUnaryOp(this, UnaryFn.Tan)
  def asin: GExpr = GUnaryOp(this, UnaryFn.Asin)
  def acos: GExpr = GUnaryOp(this, UnaryFn.Acos)
  def atan: GExpr = GUnaryOp(this, UnaryFn.Atan)
  def log: GExpr = GUnaryOp(this, UnaryFn.Log)

  // ── Scalar arithmetic (builds AST) ─────────────────────
  def +(d: Float): GExpr = GScalarOp(this, d, ScalarFn.Add)
  def -(d: Float): GExpr = GScalarOp(this, d, ScalarFn.Sub)
  def *(d: Float): GExpr = GScalarOp(this, d, ScalarFn.Mul)
  def /(d: Float): GExpr = GScalarOp(this, d, ScalarFn.Div)
  def **(power: Float): GExpr = GScalarOp(this, power, ScalarFn.Pow)

  def fma(multiply: Float, add: Float): GExpr =
    GScalarOp(GScalarOp(this, multiply, ScalarFn.Mul), add, ScalarFn.Add)

  def clamp(floor: Float, ceil: Float): GExpr =
    GClampOp(this, floor, ceil)

  // ── Elementwise binary (builds AST) ────────────────────
  def +(other: GExpr): GExpr = GBinaryOp(this, other, BinaryFn.Add)
  def -(other: GExpr): GExpr = GBinaryOp(this, other, BinaryFn.Sub)
  def *(other: GExpr): GExpr = GBinaryOp(this, other, BinaryFn.Mul)
  def /(other: GExpr): GExpr = GBinaryOp(this, other, BinaryFn.Div)

  // ── Materialize: compile AST → GPU dispatch → result ───
  def run(using CyfraRuntime): Array[Float] = GExprCompiler.run(this)

  /** Phase 1: Walk the AST and compute the dimension at each node.
    * All ops are elementwise, so propagation is trivial.
    * Catches dimension mismatches eagerly — no GPU work, no data movement.
    */
  def validateDimensions: Int = GExprCompiler.shapeAnalysis(this)
end GExpr

case class GLeaf(data: Array[Float]) extends GExpr
case class GUnaryOp(input: GExpr, fn: UnaryFn) extends GExpr
case class GScalarOp(input: GExpr, scalar: Float, fn: ScalarFn) extends GExpr
case class GClampOp(input: GExpr, floor: Float, ceil: Float) extends GExpr
case class GBinaryOp(left: GExpr, right: GExpr, fn: BinaryFn) extends GExpr

// ── Compiler: GExpr AST → Cyfra GFunction → GPU ───────────

/** Set to true to log CPU↔GPU transfer events to stderr. */
var logGExprTransfers: Boolean = false

private[gpu] object GExprCompiler:

  private def transferLog(msg: => String): Unit =
    if logGExprTransfers then System.err.println(s"[GExpr] $msg")

  /** Phase 1: Walk the AST and compute the dimension at each node.
    * All ops are elementwise, so propagation is trivial.
    * Catches dimension mismatches eagerly — no GPU work, no data movement.
    */
  def shapeAnalysis(expr: GExpr): Int =
    expr match
      case GLeaf(data)                 => data.length
      case GUnaryOp(input, _)          => shapeAnalysis(input)
      case GScalarOp(input, _, _)      => shapeAnalysis(input)
      case GClampOp(input, _, _)       => shapeAnalysis(input)
      case GBinaryOp(left, right, _)   =>
        val ld = shapeAnalysis(left)
        val rd = shapeAnalysis(right)
        if ld != rd then
          throw new IllegalArgumentException(
            s"GExpr dimension mismatch: left has $ld elements, right has $rd"
          )
        ld

  def run(expr: GExpr)(using CyfraRuntime): Array[Float] =
    shapeAnalysis(expr) // Phase 1: validate dimensions before any GPU work
    val leaves = collectLeaves(expr)
    transferLog(s"run: ${leaves.size} leaf(s), expr=${exprLabel(expr)}")
    leaves match
      case single :: Nil => runSingleInput(expr, single)
      case _             => runFused(expr, leaves)

  private def collectLeaves(expr: GExpr): List[GLeaf] =
    expr match
      case l: GLeaf              => List(l)
      case GUnaryOp(input, _)    => collectLeaves(input)
      case GScalarOp(input, _, _) => collectLeaves(input)
      case GClampOp(input, _, _) => collectLeaves(input)
      case GBinaryOp(left, right, _) =>
        val ll = collectLeaves(left)
        val rl = collectLeaves(right)
        (ll ++ rl).distinctBy(_.data.asInstanceOf[AnyRef])

  // ── Single-input path: fuse entire chain into one GFunction ──

  private def runSingleInput(expr: GExpr, leaf: GLeaf)(using CyfraRuntime): Array[Float] =
    transferLog(s"  CPU→GPU upload ${leaf.data.length} floats (runSingleInput)")
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction { (x: Float32) =>
      compileSingle(expr, leaf, x)
    }
    val result: Array[Float] = gf.run(leaf.data)
    transferLog(s"  GPU→CPU download ${result.length} floats (runSingleInput)")
    result

  private def compileSingle(expr: GExpr, leaf: GLeaf, x: Float32)(using Source): Float32 =
    expr match
      case l: GLeaf =>
        assert(l.data.asInstanceOf[AnyRef] eq leaf.data.asInstanceOf[AnyRef])
        x
      case GUnaryOp(input, fn) =>
        val inner = compileSingle(input, leaf, x)
        applyUnary(fn, inner)
      case GScalarOp(input, s, fn) =>
        val inner = compileSingle(input, leaf, x)
        applyScalar(fn, inner, s)
      case GClampOp(input, floor, ceil) =>
        val inner = compileSingle(input, leaf, x)
        F.clamp(inner, floor, ceil)
      case GBinaryOp(left, right, fn) =>
        val l = compileSingle(left, leaf, x)
        val r = compileSingle(right, leaf, x)
        applyBinary(fn, l, r)

  private def applyUnary(fn: UnaryFn, v: Float32)(using Source): Float32 =
    fn match
      case UnaryFn.Neg  => -v
      case UnaryFn.Abs  => F.abs(v)
      case UnaryFn.Exp  => F.exp(v)
      case UnaryFn.Sqrt => F.sqrt(v)
      case UnaryFn.Sin  => F.sin(v)
      case UnaryFn.Cos  => F.cos(v)
      case UnaryFn.Tan  => F.tan(v)
      case UnaryFn.Asin => F.asin(v)
      case UnaryFn.Acos => F.acos(v)
      case UnaryFn.Atan => F.atan(v)
      case UnaryFn.Log  => F.logn(v)

  private def applyScalar(fn: ScalarFn, v: Float32, s: Float)(using Source): Float32 =
    fn match
      case ScalarFn.Add => v + s
      case ScalarFn.Sub => v - s
      case ScalarFn.Mul => v * s
      case ScalarFn.Div => v / s
      case ScalarFn.Pow => F.pow(v, s)

  // ── Phase 2: Fused multi-input — single GPU dispatch ───────

  private def runFused(expr: GExpr, leaves: List[GLeaf])(using CyfraRuntime): Array[Float] =
    val n = leaves.head.data.length
    val numLeaves = leaves.size

    // Build leaf-identity → packed-buffer index
    val leafIndex: Map[AnyRef, Int] =
      leaves.zipWithIndex.map((l, i) => (l.data.asInstanceOf[AnyRef], i)).toMap

    // Pack all leaf data into one contiguous array:
    //   [leaf0[0..n-1] | leaf1[0..n-1] | ... | leafK[0..n-1]]
    val packedSize = numLeaves * n
    val packed = new Array[Float](packedSize)
    var li = 0
    while li < numLeaves do
      System.arraycopy(leaves(li).data, 0, packed, li * n, n)
      li += 1
    end while

    transferLog(s"  CPU→GPU upload $packedSize floats (runFused, $numLeaves leaves × $n)")

    val result = new Array[Float](n)

    case class FusedLayout(inputs: GBuffer[Float32], output: GBuffer[Float32]) derives Layout

    val program = GProgram.static[Int, FusedLayout](
      layout = _ => FusedLayout(
        inputs = GBuffer[Float32](packedSize),
        output = GBuffer[Float32](n)
      ),
      dispatchSize = identity
    ): layout =>
      val idx = GIO.invocationId
      val value = compileFused(expr, leafIndex, layout.inputs, idx, n)
      for _ <- layout.output.write(idx, value)
      yield GStruct.Empty()

    GBufferRegion
      .allocate[FusedLayout]
      .map: layout =>
        program.execute(n, layout)
      .runUnsafe(
        init = FusedLayout(
          inputs = GBuffer(packed),
          output = GBuffer[Float32](n)
        ),
        onDone = layout => layout.output.readArray(result)
      )
    transferLog(s"  GPU→CPU download $n floats (runFused)")
    result

  /** Walk the AST emitting Cyfra DSL code that reads each leaf from
    * the packed input buffer at offset `leafIndex * n + invocationId`.
    * All operations are fused into a single GPU kernel.
    */
  private def compileFused(
      expr: GExpr,
      leafIndex: Map[AnyRef, Int],
      inputs: GBuffer[Float32],
      idx: Int32,
      n: Int
  )(using Source): Float32 =
    expr match
      case leaf: GLeaf =>
        val i = leafIndex(leaf.data.asInstanceOf[AnyRef])
        if i == 0 then inputs.read(idx)
        else inputs.read(idx + i * n)
      case GUnaryOp(input, fn) =>
        applyUnary(fn, compileFused(input, leafIndex, inputs, idx, n))
      case GScalarOp(input, s, fn) =>
        applyScalar(fn, compileFused(input, leafIndex, inputs, idx, n), s)
      case GClampOp(input, f, c) =>
        F.clamp(compileFused(input, leafIndex, inputs, idx, n), f, c)
      case GBinaryOp(left, right, fn) =>
        val l = compileFused(left, leafIndex, inputs, idx, n)
        val r = compileFused(right, leafIndex, inputs, idx, n)
        applyBinary(fn, l, r)

  private def exprLabel(e: GExpr): String = e match
    case _: GLeaf       => "Leaf"
    case GUnaryOp(_, f) => s"Unary($f)"
    case GScalarOp(_, s, f) => s"Scalar($f, $s)"
    case GClampOp(_, lo, hi) => s"Clamp($lo, $hi)"
    case GBinaryOp(_, _, f)  => s"Binary($f)"

  private def applyBinary(fn: BinaryFn, a: Float32, b: Float32)(using Source): Float32 =
    fn match
      case BinaryFn.Add => a + b
      case BinaryFn.Sub => a - b
      case BinaryFn.Mul => a * b
      case BinaryFn.Div => a / b

end GExprCompiler

// ── Entry point: Array[Float] → GExpr ──────────────────────

extension (arr: Array[Float])
  inline def gpu: GExpr = GLeaf(arr)
