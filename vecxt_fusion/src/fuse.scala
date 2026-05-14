package vecxt.fusion

import scala.collection.mutable

/** Identifies a fusion group within a [[FusionPlan]].
  *
  * The integer `i` is the 0-based index of the group in `FusionPlan.groups`, which are stored in
  * execution order (topological).
  */
final case class GroupId(i: Int) extends AnyVal:
  override def toString: String = s"G$i"
end GroupId

/** A set of `TensorGraph` nodes that can be executed as a single fused kernel.
  *
  * All nodes in the group have their outputs produced inside the kernel; only `inputs` need to be
  * read from external buffers.
  *
  * @param nodeIds
  *   All nodes belonging to this group, in topological order (ascending `NodeId.i`).
  * @param inputs
  *   NodeIds of nodes produced *outside* this group that are consumed inside it. These are the
  *   group's kernel input buffers — either graph leaves (`Const`/`Param`/`Lift`) or
  *   multi-consumer nodes materialised by an earlier group.
  * @param output
  *   The node whose value this group exposes to downstream groups (== `nodeIds.last`).
  */
final case class FusionGroup(
    nodeIds: Vector[NodeId],
    inputs: Vector[NodeId],
    output: NodeId
)

/** The result of the fusion planner.
  *
  * Every non-leaf node of `graph` belongs to exactly one group. Leaf nodes (`Const`, `Param`,
  * `Lift`) are not assigned to any group; they are shared external inputs available to all groups.
  *
  * @param graph
  *   The original (ideally normalised) graph.
  * @param groups
  *   Fusion groups in execution order: group at index `k` only depends on groups at indices `< k`.
  * @param assignment
  *   Maps each non-leaf `NodeId` to the `GroupId` it belongs to.
  */
final case class FusionPlan(
    graph: TensorGraph,
    groups: Vector[FusionGroup],
    assignment: Map[NodeId, GroupId]
):
  /** Look up the group for a node. Returns `None` for leaves or unknown nodes. */
  def groupOf(id: NodeId): Option[GroupId] = assignment.get(id)
end FusionPlan

/** Simple stub cost model for [[FusionPlanner]].
  *
  * Provides rough FLOP and byte-transfer estimates for a [[FusionGroup]]. These numbers are
  * intentionally heuristic and are intended only to guide planner decisions (e.g. "is this node
  * too expensive to duplicate?"), not for precise performance prediction.
  *
  * Heuristics:
  *   - Most elementwise ops: 1 FLOP per output element.
  *   - Transcendental unary ops (`Exp`, `Log`, `Sin`, `Cos`, `Tan`): 20 FLOPs per element.
  *   - `Reduce`: 1 FLOP per output element (conservative; input count is ignored in the stub).
  *   - `BCast`: 0 FLOPs (pure copy / reshape).
  *   - Bytes read: 8 bytes (F64) × element count of output × number of boundary inputs.
  */
object CostModel:
  private val FLOP_TRANSCENDENTAL = 20L
  private val BYTES_PER_F64       = 8L

  /** Estimated total FLOPs for executing all nodes in `group`. */
  def estimatedFlops(group: FusionGroup, graph: TensorGraph): Long =
    group.nodeIds.foldLeft(0L)((acc, id) => acc + nodeFlops(graph(id)))

  /** Estimated bytes read by the group's kernel (boundary inputs only). */
  def estimatedBytesRead(group: FusionGroup, graph: TensorGraph): Long =
    BYTES_PER_F64 * group.inputs.size.toLong * elementCount(graph(group.output).tpe.shape)

  /** FLOP estimate for a single IR node. */
  def nodeFlops(node: TensorExpr): Long =
    val elems = elementCount(node.tpe.shape)
    node match
      case TensorExpr.Unary(op, _, _) =>
        elems * (if isTranscendental(op) then FLOP_TRANSCENDENTAL else 1L)
      case _: TensorExpr.Binary | _: TensorExpr.Cast | _: TensorExpr.Where =>
        elems
      case _: TensorExpr.BCast =>
        0L // pure reshape / copy — no arithmetic
      case _: TensorExpr.Reduce =>
        elems // stub: treat as one op per output element
      case _ =>
        0L // leaves: Const, Param, Lift

  private def isTranscendental(op: UnaryOp): Boolean = op match
    case UnaryOp.Exp | UnaryOp.Log | UnaryOp.Sin | UnaryOp.Cos | UnaryOp.Tan => true
    case _                                                                     => false

  private[fusion] def elementCount(shape: Shape): Long =
    if shape.isScalar then 1L
    else
      shape.dims.foldLeft(1L) {
        case (acc, Dim.Known(n)) => acc * n.toLong
        case (acc, _)            => acc // Sym / Unknown: treat as 1 for stub purposes
      }
end CostModel

/** Phase-6 fusion planner.
  *
  * Partitions a `TensorGraph` into `FusionGroup`s using a **top-down greedy** algorithm:
  *
  *   1. Compute reference counts: how many internal nodes consume each node.
  *   2. Walk nodes in **reverse** topological order (output-first, toward inputs).
  *   3. Each unassigned non-leaf node seeds a new group.
  *   4. BFS-absorb producers into that group when all of the following hold:
  *      a. The producer is not a leaf (`Const`/`Param`/`Lift`).
  *      b. The producer has exactly one consumer (`refCount == 1`).
  *      c. The producer is elementwise (`Unary`/`Binary`/`Where`/`Cast`/`BCast`); OR the group
  *         was seeded by a `Reduce` and has not yet absorbed any `Reduce` producer.
  *   5. Unabsorbed producers become boundary inputs to the group.
  *   6. Groups are sorted in execution order (ascending output `NodeId`).
  *
  * **Reduce placement rule:** a `Reduce` is always the terminal (output) node of its group when
  * fused. Elementwise groups (seeded by a non-Reduce node) never absorb `Reduce` producers.
  * Reduce-seeded groups absorb their elementwise producers but not a second `Reduce`.
  *
  * **Non-duplication guarantee:** nodes with `refCount > 1` are never absorbed — they are always
  * materialised and consumed across group boundaries. This prevents duplication of expensive nodes
  * such as `Exp`, `Log`, and `Reduce`.
  */
object FusionPlanner:

  /** Partition `graph` into fusion groups.
    *
    * @param graph
    *   A `TensorGraph` (preferably normalised for best fusion opportunities).
    * @return
    *   A `FusionPlan` with all non-leaf nodes assigned to a group and groups in execution order.
    */
  def plan(graph: TensorGraph): FusionPlan =
    val n = graph.size

    // ── 1. Reference counts ─────────────────────────────────────────────────
    val refCount = new Array[Int](n)
    var i        = 0
    while i < n do
      val cs = childIds(graph.nodes(i))
      var j  = 0
      while j < cs.length do
        refCount(cs(j).i) += 1
        j += 1
      end while
      i += 1
    end while

    // ── 2. Per-group mutable state ───────────────────────────────────────────
    val gNodes         = mutable.ArrayBuffer[mutable.ArrayBuffer[NodeId]]()
    val gHasReduce     = mutable.ArrayBuffer[Boolean]()
    val gSeedIsElemwise = mutable.ArrayBuffer[Boolean]()
    val gInputs        = mutable.ArrayBuffer[mutable.Set[NodeId]]()
    val assignment     = new Array[Int](n)
    java.util.Arrays.fill(assignment, -1)

    def newGroup(): Int =
      val gid = gNodes.size
      gNodes         += mutable.ArrayBuffer[NodeId]()
      gHasReduce     += false
      gSeedIsElemwise += true // overwritten by absorb for the seed
      gInputs        += mutable.HashSet[NodeId]()
      gid

    def absorb(id: NodeId, gid: Int, isSeed: Boolean): Unit =
      assignment(id.i) = gid
      gNodes(gid) += id
      val isReduce = graph(id).isInstanceOf[TensorExpr.Reduce]
      if isReduce then gHasReduce(gid) = true
      if isSeed then gSeedIsElemwise(gid) = !isReduce

    // ── 3. Reverse topological walk + greedy BFS absorption ─────────────────
    i = n - 1
    while i >= 0 do
      val id   = NodeId(i)
      val node = graph(id)
      if !isLeaf(node) && assignment(id.i) < 0 then
        val gid = newGroup()
        absorb(id, gid, isSeed = true)

        val worklist = mutable.Queue[NodeId]()
        val enqueued = mutable.HashSet[Int]()

        def enqueue(c: NodeId): Unit =
          if !enqueued.contains(c.i) then
            enqueued += c.i
            worklist.enqueue(c)

        val cs0 = childIds(node)
        var j   = 0
        while j < cs0.length do
          enqueue(cs0(j))
          j += 1
        end while

        while worklist.nonEmpty do
          val prodId = worklist.dequeue()
          val prod   = graph(prodId)

          if isLeaf(prod) then
            gInputs(gid) += prodId
          else if assignment(prodId.i) >= 0 then
            gInputs(gid) += prodId // already in another group: boundary
          else if refCount(prodId.i) > 1 then
            gInputs(gid) += prodId // multi-consumer barrier
          else if prod.isInstanceOf[TensorExpr.Reduce] &&
            (gHasReduce(gid) || gSeedIsElemwise(gid))
          then
            gInputs(gid) += prodId // can't absorb: second reduce, or elemwise-seeded group
          else
            absorb(prodId, gid, isSeed = false)
            val cs = childIds(prod)
            var k  = 0
            while k < cs.length do
              enqueue(cs(k))
              k += 1
            end while
          end if
        end while
      end if
      i -= 1
    end while

    // ── 4. Assemble FusionGroups in execution order ──────────────────────────
    val numGroups    = gNodes.size
    val sortedOldGids = (0 until numGroups).sortBy(gid => gNodes(gid).maxBy(_.i).i)
    val oldToNew     = sortedOldGids.zipWithIndex.toMap

    val groups = sortedOldGids.map { oldGid =>
      val sortedIds = gNodes(oldGid).sortBy(_.i).toVector
      val inputs    = gInputs(oldGid).toVector.sortBy(_.i)
      FusionGroup(sortedIds, inputs, sortedIds.last)
    }.toVector

    // ── 5. Build final assignment map (new group ids) ────────────────────────
    val assignMap = (0 until n)
      .filter(i => assignment(i) >= 0)
      .map(i => NodeId(i) -> GroupId(oldToNew(assignment(i))))
      .toMap

    FusionPlan(graph, groups, assignMap)
  end plan

  // ── Internal helpers ──────────────────────────────────────────────────────

  private def isLeaf(node: TensorExpr): Boolean = node match
    case _: TensorExpr.Const | _: TensorExpr.Param | _: TensorExpr.Lift => true
    case _                                                                => false

  /** Direct producer `NodeId`s of `node` (its children in the DAG). */
  private[fusion] def childIds(node: TensorExpr): Vector[NodeId] = node match
    case TensorExpr.Unary(_, a, _)      => Vector(a)
    case TensorExpr.Binary(_, a, b, _)  => Vector(a, b)
    case TensorExpr.Cast(_, a, _)       => Vector(a)
    case TensorExpr.BCast(a, _, _)      => Vector(a)
    case TensorExpr.Reduce(_, a, _, _)  => Vector(a)
    case TensorExpr.Where(c, x, y, _)   => Vector(c, x, y)
    case _                              => Vector.empty

end FusionPlanner
