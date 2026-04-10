package vecxt.gpu

import io.computenode.cyfra.core.CyfraRuntime

// ── GNDExpr: N-dimensional expression tree for GPU ─────────

/** An N-dimensional GPU expression that tracks shape and strides.
  *
  * All shape/stride metadata lives on the CPU. The backing data is a flat
  * `Array[Float]` that is only uploaded to GPU at materialisation time.
  *
  * Column-major by default (matching vecxt.NDArray conventions).
  */
sealed trait GNDExpr:

  // ── Unary elementwise (builds AST, no GPU work) ────────
  def neg: GNDExpr = GNDUnaryOp(this, UnaryFn.Neg)
  def abs: GNDExpr = GNDUnaryOp(this, UnaryFn.Abs)
  def exp: GNDExpr = GNDUnaryOp(this, UnaryFn.Exp)
  def sqrt: GNDExpr = GNDUnaryOp(this, UnaryFn.Sqrt)
  def sin: GNDExpr = GNDUnaryOp(this, UnaryFn.Sin)
  def cos: GNDExpr = GNDUnaryOp(this, UnaryFn.Cos)
  def tan: GNDExpr = GNDUnaryOp(this, UnaryFn.Tan)
  def asin: GNDExpr = GNDUnaryOp(this, UnaryFn.Asin)
  def acos: GNDExpr = GNDUnaryOp(this, UnaryFn.Acos)
  def atan: GNDExpr = GNDUnaryOp(this, UnaryFn.Atan)
  def log: GNDExpr = GNDUnaryOp(this, UnaryFn.Log)

  // ── Scalar arithmetic (builds AST) ─────────────────────
  def +(d: Float): GNDExpr = GNDScalarOp(this, d, ScalarFn.Add)
  def -(d: Float): GNDExpr = GNDScalarOp(this, d, ScalarFn.Sub)
  def *(d: Float): GNDExpr = GNDScalarOp(this, d, ScalarFn.Mul)
  def /(d: Float): GNDExpr = GNDScalarOp(this, d, ScalarFn.Div)
  def **(power: Float): GNDExpr = GNDScalarOp(this, power, ScalarFn.Pow)

  def clamp(floor: Float, ceil: Float): GNDExpr =
    GNDClampOp(this, floor, ceil)

  // ── Elementwise binary — requires matching shapes ──────
  def +(other: GNDExpr): GNDExpr = GNDBinaryOp(this, other, BinaryFn.Add)
  def -(other: GNDExpr): GNDExpr = GNDBinaryOp(this, other, BinaryFn.Sub)
  def *(other: GNDExpr): GNDExpr = GNDBinaryOp(this, other, BinaryFn.Mul)
  def /(other: GNDExpr): GNDExpr = GNDBinaryOp(this, other, BinaryFn.Div)

  // ── Explicit broadcast (builds AST, zero-copy) ────────
  def broadcastTo(targetShape: Array[Int]): GNDExpr = GNDBroadcastOp(this, targetShape.clone())

  // ── Matrix multiply (builds AST) ──────────────────────
  def @@(other: GNDExpr): GNDExpr = GNDMatMulOp(this, other)

  // ── Reshape / transpose (builds AST, zero-copy) ───────
  def reshape(newShape: Array[Int]): GNDExpr = GNDReshapeOp(this, newShape.clone())
  def T: GNDExpr = GNDTransposeOp(this)

  /** Phase 1: validate shapes across the whole tree. Returns the output shape. */
  def validateShape: Array[Int] = GNDShapeAnalysis.analyze(this)

  /** Lower to GExpr, dispatch to GPU, wrap result back as GNDLeaf.
    * Requires all leaves to be contiguous col-major with offset=0.
    * Throws UnsupportedOperationException for broadcast, matmul, transpose.
    */
  def run(using CyfraRuntime): GNDLeaf = GNDExprCompiler.run(this)

  /** Fused CPU evaluation: walks the AST per-element in a single pass.
    * No intermediate arrays — constant memory regardless of pipeline depth.
    * Does not require a CyfraRuntime.
    */
  def runCpu: GNDLeaf = GNDExprCompiler.runCpu(this)
end GNDExpr

// ── AST node types ─────────────────────────────────────────

/** Leaf: raw data with shape, strides, offset — models a strided view. */
case class GNDLeaf(
    data: Array[Float],
    shape: Array[Int],
    strides: Array[Int],
    offset: Int
) extends GNDExpr

case class GNDUnaryOp(input: GNDExpr, fn: UnaryFn) extends GNDExpr
case class GNDScalarOp(input: GNDExpr, scalar: Float, fn: ScalarFn) extends GNDExpr
case class GNDClampOp(input: GNDExpr, floor: Float, ceil: Float) extends GNDExpr
case class GNDBinaryOp(left: GNDExpr, right: GNDExpr, fn: BinaryFn) extends GNDExpr
case class GNDBroadcastOp(input: GNDExpr, targetShape: Array[Int]) extends GNDExpr
case class GNDMatMulOp(left: GNDExpr, right: GNDExpr) extends GNDExpr
case class GNDReshapeOp(input: GNDExpr, newShape: Array[Int]) extends GNDExpr
case class GNDTransposeOp(input: GNDExpr) extends GNDExpr

// ── Shape analysis: pure CPU, no GPU work ──────────────────

object GNDShapeAnalysis:

  /** Walk the AST and compute the output shape at each node.
    *
    * Rules:
    *  - Leaf         → leaf.shape
    *  - Unary/Scalar/Clamp → same shape as input
    *  - Binary       → require exact shape match (use broadcastTo first)
    *  - Broadcast    → validate broadcast compatibility, return targetShape
    *  - MatMul       → standard matmul shape rule: [..., M, K] @@ [..., K, N] → [..., M, N]
    *  - Reshape      → validate numel match, return newShape
    *  - Transpose    → reversed shape
    *
    * Throws `IllegalArgumentException` on any incompatibility.
    */
  def analyze(expr: GNDExpr): Array[Int] =
    expr match
      case GNDLeaf(_, shape, _, _)       => shape.clone()
      case GNDUnaryOp(input, _)          => analyze(input)
      case GNDScalarOp(input, _, _)      => analyze(input)
      case GNDClampOp(input, _, _)       => analyze(input)
      case GNDBinaryOp(left, right, _)   =>
        val ls = analyze(left)
        val rs = analyze(right)
        requireSameShape(ls, rs)
        ls
      case GNDBroadcastOp(input, targetShape) =>
        val is = analyze(input)
        validateBroadcast(is, targetShape)
        targetShape.clone()
      case GNDMatMulOp(left, right)      =>
        val ls = analyze(left)
        val rs = analyze(right)
        matmulShapeOrThrow(ls, rs)
      case GNDReshapeOp(input, newShape) =>
        val is = analyze(input)
        val inNumel = numel(is)
        val outNumel = numel(newShape)
        if inNumel != outNumel then
          throw new IllegalArgumentException(
            s"GNDExpr reshape: cannot reshape [${is.mkString(",")}] ($inNumel elements) " +
              s"to [${newShape.mkString(",")}] ($outNumel elements)"
          )
        newShape.clone()
      case GNDTransposeOp(input) =>
        val is = analyze(input)
        if is.length < 2 then
          throw new IllegalArgumentException(
            s"GNDExpr transpose requires at least 2 dimensions, got [${is.mkString(",")}]"
          )
        is.reverse

  // ── Shape matching for binary ops ────────────────────────

  /** Require that two shapes are identical rank and element-wise equal. */
  private def requireSameShape(a: Array[Int], b: Array[Int]): Unit =
    if a.length != b.length then
      throw new IllegalArgumentException(
        s"GNDExpr binary: shape mismatch — [${a.mkString(",")}] vs [${b.mkString(",")}] " +
          s"(rank ${a.length} != ${b.length}). Use .broadcastTo to align shapes explicitly."
      )
    var i = 0
    while i < a.length do
      if a(i) != b(i) then
        throw new IllegalArgumentException(
          s"GNDExpr binary: shape mismatch — [${a.mkString(",")}] vs [${b.mkString(",")}] " +
            s"at dimension $i (${a(i)} vs ${b(i)}). Use .broadcastTo to align shapes explicitly."
        )
      i += 1
    end while

  // ── Broadcast validation ─────────────────────────────────

  /** Validate that `inputShape` can be broadcast to `targetShape`. */
  private[gpu] def validateBroadcast(inputShape: Array[Int], targetShape: Array[Int]): Unit =
    if inputShape.length > targetShape.length then
      throw new IllegalArgumentException(
        s"GNDExpr broadcast: cannot broadcast [${inputShape.mkString(",")}] to [${targetShape.mkString(",")}] — " +
          s"source has more dimensions (${inputShape.length}) than target (${targetShape.length})"
      )
    val n = targetShape.length
    var i = 0
    while i < n do
      val srcIdx = i - (n - inputShape.length)
      val srcDim = if srcIdx < 0 then 1 else inputShape(srcIdx)
      val tgtDim = targetShape(i)
      if srcDim != tgtDim && srcDim != 1 then
        throw new IllegalArgumentException(
          s"GNDExpr broadcast: cannot broadcast [${inputShape.mkString(",")}] to [${targetShape.mkString(",")}] — " +
            s"incompatible at dimension $i ($srcDim vs $tgtDim)"
        )
      i += 1
    end while

  /** Compute broadcast output shape for two shapes. Used internally by matmul batch dims. */
  private[gpu] def broadcastShapeOrThrow(a: Array[Int], b: Array[Int]): Array[Int] =
    val n = math.max(a.length, b.length)
    val out = new Array[Int](n)
    var i = 0
    while i < n do
      val ai = i - (n - a.length)
      val bi = i - (n - b.length)
      val da = if ai < 0 then 1 else a(ai)
      val db = if bi < 0 then 1 else b(bi)
      if da == db then out(i) = da
      else if da == 1 then out(i) = db
      else if db == 1 then out(i) = da
      else
        throw new IllegalArgumentException(
          s"GNDExpr broadcast: incompatible shapes [${a.mkString(",")}] and [${b.mkString(",")}] " +
            s"at dimension $i ($da vs $db)"
        )
      i += 1
    end while
    out

  // ── MatMul shape ─────────────────────────────────────────

  /** Compute matmul output shape.
    *
    * Supports:
    *  - 2D × 2D: [M,K] @@ [K,N] → [M,N]
    *  - Batched:  [...,M,K] @@ [...,K,N] → [...,M,N]
    *    where batch dims are broadcast.
    *  - 1D × 2D: [K] @@ [K,N] → [N]        (vector-matrix)
    *  - 2D × 1D: [M,K] @@ [K] → [M]        (matrix-vector)
    *  - 1D × 1D: [K] @@ [K] → []            (dot product → scalar)
    */
  private[gpu] def matmulShapeOrThrow(a: Array[Int], b: Array[Int]): Array[Int] =
    // Handle 1D cases by temporarily promoting to 2D
    val (aEff, aSqueezeFront) = if a.length == 1 then (Array(1, a(0)), true) else (a, false)
    val (bEff, aSqueezeBack) = if b.length == 1 then (Array(b(0), 1), true) else (b, false)

    val aRank = aEff.length
    val bRank = bEff.length
    val m = aEff(aRank - 2)
    val k1 = aEff(aRank - 1)
    val k2 = bEff(bRank - 2)
    val n = bEff(bRank - 1)

    if k1 != k2 then
      throw new IllegalArgumentException(
        s"GNDExpr matmul: inner dimensions don't match — " +
          s"[${a.mkString(",")}] @@ [${b.mkString(",")}]: $k1 != $k2"
      )

    // Broadcast batch dims (everything except last 2)
    val aBatch = aEff.take(aRank - 2)
    val bBatch = bEff.take(bRank - 2)
    val batchShape = if aBatch.isEmpty && bBatch.isEmpty then Array.empty[Int]
                     else broadcastShapeOrThrow(aBatch, bBatch)

    // Build output shape, then un-squeeze if we promoted 1D inputs
    val fullShape = batchShape ++ Array(m, n)

    // Undo the 1D promotions
    if aSqueezeFront && aSqueezeBack then
      fullShape.drop(fullShape.length - 2).dropRight(1) // scalar [] — but represented as empty
      Array.empty[Int]
    else if aSqueezeFront then
      // Remove the M=1 dim from output
      batchShape ++ Array(n)
    else if aSqueezeBack then
      // Remove the N=1 dim from output
      batchShape ++ Array(m)
    else
      fullShape
  end matmulShapeOrThrow

  private[gpu] def numel(shape: Array[Int]): Int =
    if shape.isEmpty then 1
    else
      var prod = 1
      var i = 0
      while i < shape.length do
        prod *= shape(i)
        i += 1
      end while
      prod

end GNDShapeAnalysis

// ── Entry points ───────────────────────────────────────────

/** Construct a GNDLeaf from a flat array and a shape, using column-major strides. */
object GNDArray:

  def apply(data: Array[Float], shape: Array[Int]): GNDLeaf =
    val strides = colMajorStrides(shape)
    requireValidLeaf(data, shape, strides, 0)
    GNDLeaf(data, shape.clone(), strides, 0)

  def apply(data: Array[Float], shape: Array[Int], strides: Array[Int], offset: Int = 0): GNDLeaf =
    requireValidLeaf(data, shape, strides, offset)
    GNDLeaf(data, shape.clone(), strides.clone(), offset)

  /** 1D convenience. */
  def fromArray(data: Array[Float]): GNDLeaf =
    GNDLeaf(data, Array(data.length), Array(1), 0)

  /** 2D convenience: matrix with rows × cols, column-major. */
  def matrix(data: Array[Float], rows: Int, cols: Int): GNDLeaf =
    apply(data, Array(rows, cols))

  private[gpu] def colMajorStrides(shape: Array[Int]): Array[Int] =
    val s = new Array[Int](shape.length)
    if shape.nonEmpty then
      s(0) = 1
      var i = 1
      while i < shape.length do
        s(i) = s(i - 1) * shape(i - 1)
        i += 1
      end while
    end if
    s

  private def requireValidLeaf(data: Array[Float], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    if shape.length != strides.length then
      throw new IllegalArgumentException(
        s"GNDArray: shape rank ${shape.length} != strides rank ${strides.length}"
      )
    if shape.exists(_ < 0) then
      throw new IllegalArgumentException(
        s"GNDArray: shape has negative dimension: [${shape.mkString(",")}]"
      )
    if offset < 0 then
      throw new IllegalArgumentException(s"GNDArray: negative offset $offset")
    // Check that every reachable index stays within data bounds
    if shape.nonEmpty then
      var maxIdx = offset
      var i = 0
      while i < shape.length do
        if shape(i) > 0 then maxIdx += (shape(i) - 1) * math.abs(strides(i))
        i += 1
      end while
      if maxIdx >= data.length then
        throw new IllegalArgumentException(
          s"GNDArray: max reachable index $maxIdx >= data.length ${data.length} " +
            s"(shape=[${shape.mkString(",")}], strides=[${strides.mkString(",")}], offset=$offset)"
        )
    end if
  end requireValidLeaf

end GNDArray

// ── GNDExpr compiler: lower to GExpr for GPU dispatch ──────

/** Set to true to log GNDExpr CPU↔GPU transfer events to stderr. */
var logGNDExprTransfers: Boolean = false

private[gpu] object GNDExprCompiler:

  private def transferLog(msg: => String): Unit =
    if logGNDExprTransfers then System.err.println(s"[GNDExpr] $msg")

  /** Validate shapes, lower to GExpr, run on GPU, wrap result. */
  def run(expr: GNDExpr)(using CyfraRuntime): GNDLeaf =
    val outShape = GNDShapeAnalysis.analyze(expr)
    val n = GNDShapeAnalysis.numel(outShape)
    transferLog(s"run: shape=[${outShape.mkString(",")}] ($n elements), lowering to GExpr")
    val lowered = lower(expr)
    transferLog(s"  lowered — delegating to GExprCompiler")
    val result = GExprCompiler.run(lowered)
    transferLog(s"  GPU→CPU done, wrapping $n floats as GNDLeaf shape=[${outShape.mkString(",")}]")
    val strides = GNDArray.colMajorStrides(outShape)
    GNDLeaf(result, outShape, strides, 0)

  /** Recursively lower a GNDExpr tree to a flat GExpr tree.
    *
    * Elementwise ops map 1:1. Reshape is a no-op (same flat data).
    * Broadcast, matmul, and transpose throw — they require non-trivial
    * GPU kernels that the 1D GExpr path doesn't support yet.
    */
  private def lower(expr: GNDExpr): GExpr =
    expr match
      case leaf: GNDLeaf                  =>
        requireContiguous(leaf)
        transferLog(s"  lower: GNDLeaf(${leaf.data.length} floats, shape=[${leaf.shape.mkString(",")}]) → GLeaf")
        GLeaf(leaf.data)
      case GNDUnaryOp(input, fn)          => GUnaryOp(lower(input), fn)
      case GNDScalarOp(input, s, fn)      => GScalarOp(lower(input), s, fn)
      case GNDClampOp(input, f, c)        => GClampOp(lower(input), f, c)
      case GNDBinaryOp(left, right, fn)   => GBinaryOp(lower(left), lower(right), fn)
      case GNDReshapeOp(input, _)         => lower(input) // same flat data
      case _: GNDBroadcastOp =>
        throw new UnsupportedOperationException(
          "GNDExpr broadcast lowering to GExpr is not yet supported"
        )
      case _: GNDMatMulOp =>
        throw new UnsupportedOperationException(
          "GNDExpr matmul lowering to GExpr is not yet supported"
        )
      case _: GNDTransposeOp =>
        throw new UnsupportedOperationException(
          "GNDExpr transpose lowering to GExpr is not yet supported — non-contiguous layout"
        )

  /** Ensure a leaf has offset=0, col-major strides, and data.length == numel. */
  private def requireContiguous(leaf: GNDLeaf): Unit =
    if leaf.offset != 0 then
      throw new UnsupportedOperationException(
        s"GNDExpr lowering requires offset=0, got ${leaf.offset}"
      )
    val expected = GNDArray.colMajorStrides(leaf.shape)
    if !java.util.Arrays.equals(leaf.strides, expected) then
      throw new UnsupportedOperationException(
        s"GNDExpr lowering requires contiguous col-major strides [${expected.mkString(",")}], " +
          s"got [${leaf.strides.mkString(",")}]"
      )
    val n = GNDShapeAnalysis.numel(leaf.shape)
    if leaf.data.length != n then
      throw new UnsupportedOperationException(
        s"GNDExpr lowering requires data.length == numel(shape) ($n), got ${leaf.data.length}"
      )

  // ── Fused CPU backend ──────────────────────────────────

  /** Fused CPU evaluation: single pass, no intermediate arrays. */
  def runCpu(expr: GNDExpr): GNDLeaf =
    val outShape = GNDShapeAnalysis.analyze(expr)
    val n = GNDShapeAnalysis.numel(outShape)
    val out = new Array[Float](n)
    var i = 0
    while i < n do
      out(i) = evalElement(expr, i)
      i += 1
    end while
    val strides = GNDArray.colMajorStrides(outShape)
    GNDLeaf(out, outShape, strides, 0)

  /** Recursively evaluate the expression tree for a single flat index. */
  private def evalElement(expr: GNDExpr, i: Int): Float =
    expr match
      case leaf: GNDLeaf =>
        leaf.data(leaf.offset + i) // contiguous col-major: flat index == data index
      case GNDUnaryOp(input, fn) =>
        val v = evalElement(input, i)
        cpuUnary(fn, v)
      case GNDScalarOp(input, s, fn) =>
        val v = evalElement(input, i)
        cpuScalar(fn, v, s)
      case GNDClampOp(input, floor, ceil) =>
        val v = evalElement(input, i)
        math.max(floor, math.min(ceil, v))
      case GNDBinaryOp(left, right, fn) =>
        val l = evalElement(left, i)
        val r = evalElement(right, i)
        cpuBinary(fn, l, r)
      case GNDReshapeOp(input, _) =>
        evalElement(input, i) // same flat data
      case _: GNDBroadcastOp =>
        throw new UnsupportedOperationException("runCpu: broadcast not yet supported")
      case _: GNDMatMulOp =>
        throw new UnsupportedOperationException("runCpu: matmul not yet supported")
      case _: GNDTransposeOp =>
        throw new UnsupportedOperationException("runCpu: transpose not yet supported")

  private def cpuUnary(fn: UnaryFn, v: Float): Float =
    fn match
      case UnaryFn.Neg  => -v
      case UnaryFn.Abs  => math.abs(v)
      case UnaryFn.Exp  => math.exp(v.toDouble).toFloat
      case UnaryFn.Sqrt => math.sqrt(v.toDouble).toFloat
      case UnaryFn.Sin  => math.sin(v.toDouble).toFloat
      case UnaryFn.Cos  => math.cos(v.toDouble).toFloat
      case UnaryFn.Tan  => math.tan(v.toDouble).toFloat
      case UnaryFn.Asin => math.asin(v.toDouble).toFloat
      case UnaryFn.Acos => math.acos(v.toDouble).toFloat
      case UnaryFn.Atan => math.atan(v.toDouble).toFloat
      case UnaryFn.Log  => math.log(v.toDouble).toFloat

  private def cpuScalar(fn: ScalarFn, v: Float, s: Float): Float =
    fn match
      case ScalarFn.Add => v + s
      case ScalarFn.Sub => v - s
      case ScalarFn.Mul => v * s
      case ScalarFn.Div => v / s
      case ScalarFn.Pow => math.pow(v.toDouble, s.toDouble).toFloat

  private def cpuBinary(fn: BinaryFn, a: Float, b: Float): Float =
    fn match
      case BinaryFn.Add => a + b
      case BinaryFn.Sub => a - b
      case BinaryFn.Mul => a * b
      case BinaryFn.Div => a / b

end GNDExprCompiler
