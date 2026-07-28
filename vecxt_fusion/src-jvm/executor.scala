package vecxt.fusion

import scala.math

/** JVM Phase-8 kernel executor — while-loop closures over `Array[Double]`.
  *
  * Interprets a `CompiledKernel` by walking the `ScalarExpr` tree at each element index. Only F64 (Double) data is
  * supported. The second cut (SIMD via `jdk.incubator.vector`) will replace the inner loop for elementwise groups in a
  * later phase.
  *
  * ===Scalar encoding conventions===
  *
  *   - Boolean values are represented as `Double`: `0.0` = false, any other value = true.
  *   - `ArgMax`/`ArgMin` return the flat index as a `Double` in a length-1 output array.
  */
object KernelExecutor:

  /** Execute `kernel` given pre-materialised F64 input arrays and return the output buffer.
    *
    * @param kernel
    *   the compiled kernel to execute
    * @param inputs
    *   one flat `Array[Double]` per `kernel.inputNodes` entry, in the same order. Each array must have at least
    *   `kernel.ir.inputNumel(k)` elements.
    * @return
    *   the output buffer as a flat, contiguous, column-major `Array[Double]`
    */
  def run(kernel: CompiledKernel, inputs: Array[Array[Double]]): Array[Double] =
    kernel.ir match
      case e: KernelIR.Elementwise => runElementwise(e, inputs)
      case r: KernelIR.FullReduce  => runFullReduce(r, inputs)
  end run

  // ── Elementwise ─────────────────────────────────────────────────────────────

  private def runElementwise(ir: KernelIR.Elementwise, inputs: Array[Array[Double]]): Array[Double] =
    val n = ir.outShape.foldLeft(1)(_ * _)
    val out = new Array[Double](n)
    var i = 0
    while i < n do
      out(i) = evalScalar(ir.expr, i, inputs)
      i += 1
    end while
    out
  end runElementwise

  // ── FullReduce ───────────────────────────────────────────────────────────────

  private def runFullReduce(ir: KernelIR.FullReduce, inputs: Array[Array[Double]]): Array[Double] =
    import ReduceOp.*
    val n = ir.inNumel
    ir.op match

      case Sum =>
        var acc = 0.0
        var i = 0
        while i < n do
          acc += evalScalar(ir.bodyExpr, i, inputs)
          i += 1
        end while
        Array(acc)

      case Product =>
        var acc = 1.0
        var i = 0
        while i < n do
          acc *= evalScalar(ir.bodyExpr, i, inputs)
          i += 1
        end while
        Array(acc)

      case Min =>
        var acc = Double.PositiveInfinity
        var i = 0
        while i < n do
          val v = evalScalar(ir.bodyExpr, i, inputs)
          if v < acc then acc = v
          end if
          i += 1
        end while
        Array(acc)

      case Max =>
        var acc = Double.NegativeInfinity
        var i = 0
        while i < n do
          val v = evalScalar(ir.bodyExpr, i, inputs)
          if v > acc then acc = v
          end if
          i += 1
        end while
        Array(acc)

      case All =>
        var acc = true
        var i = 0
        while i < n && acc do
          acc = evalScalar(ir.bodyExpr, i, inputs) != 0.0
          i += 1
        end while
        Array(if acc then 1.0 else 0.0)

      case Any =>
        var acc = false
        var i = 0
        while i < n && !acc do
          acc = evalScalar(ir.bodyExpr, i, inputs) != 0.0
          i += 1
        end while
        Array(if acc then 1.0 else 0.0)

      case ArgMax =>
        var best = Double.NegativeInfinity
        var bestIdx = 0
        var i = 0
        while i < n do
          val v = evalScalar(ir.bodyExpr, i, inputs)
          if v > best then
            best = v
            bestIdx = i
          end if
          i += 1
        end while
        Array(bestIdx.toDouble)

      case ArgMin =>
        var best = Double.PositiveInfinity
        var bestIdx = 0
        var i = 0
        while i < n do
          val v = evalScalar(ir.bodyExpr, i, inputs)
          if v < best then
            best = v
            bestIdx = i
          end if
          i += 1
        end while
        Array(bestIdx.toDouble)

    end match
  end runFullReduce

  // ── Scalar evaluation ────────────────────────────────────────────────────────

  /** Evaluate a `ScalarExpr` at flat element index `i` given the kernel's input arrays. */
  private def evalScalar(expr: ScalarExpr, i: Int, inputs: Array[Array[Double]]): Double =
    expr match
      case ScalarExpr.Load(BufRef.Input(k), numel) =>
        inputs(k)(i % numel)

      case ScalarExpr.Lit(v) =>
        v

      case ScalarExpr.SUnary(op, a) =>
        applyUnary(op, evalScalar(a, i, inputs))

      case ScalarExpr.SBinary(op, a, b) =>
        applyBinary(op, evalScalar(a, i, inputs), evalScalar(b, i, inputs))

      case ScalarExpr.Select(c, x, y) =>
        if evalScalar(c, i, inputs) != 0.0 then evalScalar(x, i, inputs)
        else evalScalar(y, i, inputs)

  end evalScalar

  // ── Op dispatch ──────────────────────────────────────────────────────────────

  private def applyUnary(op: UnaryOp, a: Double): Double =
    import UnaryOp.*
    op match
      case Neg        => -a
      case Sin        => math.sin(a)
      case Cos        => math.cos(a)
      case Tan        => math.tan(a)
      case Exp        => math.exp(a)
      case Log        => math.log(a)
      case Sqrt       => math.sqrt(a)
      case Abs        => math.abs(a)
      case Not        => if a != 0.0 then 0.0 else 1.0
      case Reciprocal => 1.0 / a
    end match
  end applyUnary

  private def applyBinary(op: BinaryOp, a: Double, b: Double): Double =
    import BinaryOp.*
    op match
      case Add => a + b
      case Sub => a - b
      case Mul => a * b
      case Div => a / b
      case Pow => math.pow(a, b)
      case Min => math.min(a, b)
      case Max => math.max(a, b)
      case Eq  => if a == b then 1.0 else 0.0
      case Neq => if a != b then 1.0 else 0.0
      case Lt  => if a < b then 1.0 else 0.0
      case Lte => if a <= b then 1.0 else 0.0
      case Gt  => if a > b then 1.0 else 0.0
      case Gte => if a >= b then 1.0 else 0.0
      case And => if a != 0.0 && b != 0.0 then 1.0 else 0.0
      case Or  => if a != 0.0 || b != 0.0 then 1.0 else 0.0
    end match
  end applyBinary

end KernelExecutor
