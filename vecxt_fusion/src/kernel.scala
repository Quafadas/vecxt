package vecxt.fusion

/** Reference to a buffer consumed by a kernel.
  *
  * Every kernel has a fixed number of input buffers and exactly one output buffer.
  */
enum BufRef:
  /** The k-th input buffer (0-indexed). The corresponding `NDArray` data is provided at execution time. */
  case Input(idx: Int)
end BufRef

/** A scalar computation tree evaluated at a single element index `i`.
  *
  * Each node computes one `Double` value. The index `i` is a flat, 0-based, column-major element position shared by all
  * buffers in the kernel unless overridden by broadcast-safe wrapping.
  */
sealed trait ScalarExpr

object ScalarExpr:

  /** Load element `i % numel` from input buffer `buf`.
    *
    * Using `i % numel` rather than plain `i` makes broadcast transparent: if the source buffer has fewer elements than
    * the output (because it was broadcast), the modulo wraps the index correctly. When `numel == outNumel` the modulo
    * is free for the JIT to eliminate.
    *
    * @param buf
    *   the input buffer to read from
    * @param numel
    *   total elements in the source buffer
    */
  final case class Load(buf: BufRef.Input, numel: Int) extends ScalarExpr

  /** A compile-time constant value (comes from a `TensorExpr.Const` node). */
  final case class Lit(value: Double) extends ScalarExpr

  /** Apply a unary op pointwise. */
  final case class SUnary(op: UnaryOp, a: ScalarExpr) extends ScalarExpr

  /** Apply a binary op pointwise. */
  final case class SBinary(op: BinaryOp, a: ScalarExpr, b: ScalarExpr) extends ScalarExpr

  /** Conditional select: `if c != 0.0 then x else y`. */
  final case class Select(c: ScalarExpr, x: ScalarExpr, y: ScalarExpr) extends ScalarExpr

end ScalarExpr

/** A low-level computation kernel produced by `Schedule.lower` for one `FusionGroup`.
  *
  * The kernel is a pure data structure — a description of the computation. Backends (JVM while-loop, SIMD, JS, Native)
  * interpret or compile it to produce actual execution.
  */
sealed trait KernelIR:
  /** Number of input buffers required by this kernel. */
  def inputCount: Int
end KernelIR

object KernelIR:

  /** Elementwise kernel: computes `out[i] = expr(inputs[0][i % n0], inputs[1][i % n1], …)` for `i` in
    * `0 until outNumel`.
    *
    * @param outShape
    *   shape of the output tensor; determines the loop bound `outNumel = product(outShape)`.
    * @param inputNumel
    *   number of elements in each input buffer, in the same order as `CompiledKernel.inputNodes`. Used for
    *   broadcast-safe index wrapping inside `ScalarExpr.Load`.
    * @param expr
    *   the per-element scalar computation tree.
    */
  final case class Elementwise(
      outShape: Array[Int],
      inputNumel: Array[Int],
      expr: ScalarExpr
  ) extends KernelIR:
    def inputCount: Int = inputNumel.length
  end Elementwise

  /** Full-tensor reduction: reduces `bodyExpr(inputs, i)` for every `i` in `0 until inNumel` into a single scalar using
    * `op`.
    *
    * Only full reductions (all axes, scalar output) are supported in Phase 8. Partial-axis reductions will be added in
    * a later phase.
    *
    * @param inNumel
    *   number of elements to iterate over (= product of input shape).
    * @param inputNumel
    *   element counts for each input buffer (for broadcast-safe `Load`).
    * @param op
    *   the reduction operation.
    * @param bodyExpr
    *   per-element scalar expression that computes the value to fold.
    */
  final case class FullReduce(
      inNumel: Int,
      inputNumel: Array[Int],
      op: ReduceOp,
      bodyExpr: ScalarExpr
  ) extends KernelIR:
    def inputCount: Int = inputNumel.length
  end FullReduce

end KernelIR

/** A fully scheduled, executable kernel with its input wiring.
  *
  * @param ir
  *   the computation to perform.
  * @param inputNodes
  *   the `NodeId`s (in the original normalised `TensorGraph`) that supply each input buffer, in the same order as
  *   `ir.inputNumel` / `BufRef.Input(k)`. An executor resolves these to actual arrays at run time.
  * @param outputNode
  *   the `NodeId` whose value this kernel produces. An executor stores its result under this id.
  */
final case class CompiledKernel(
    ir: KernelIR,
    inputNodes: Vector[NodeId],
    outputNode: NodeId
)
