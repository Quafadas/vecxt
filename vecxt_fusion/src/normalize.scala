package vecxt.fusion

import scala.collection.mutable

/** Phase-5 normalisation pass over a `TensorGraph`.
  *
  * Applies the following local rewrite rules in a single DAG-walk:
  *
  *   - **Constant folding** — numeric `Unary`/`Binary` nodes whose operands are all `Const` scalars are collapsed to a
  *     single `Const`. Supported dtypes: `F64` operands → `F64`/`Bool` result; `Bool` operands → `Bool` result.
  *   - **Identity removal** — `x + 0 → x`, `x - 0 → x`, `x * 1 → x`, `x / 1 → x`, `pow(x, 1) → x`.
  *   - **Annihilator removal** — `x * 0 → 0`, `0 / x → 0` (when `x` is a non-zero constant; `0 / 0` is left as-is to
  *     avoid changing NaN/exception semantics), `pow(x, 0) → 1`, `pow(1, x) → 1`.
  *   - **`Where` short-circuit** — `where(true, x, y) → x`; `where(false, x, y) → y`.
  *   - **`Cast` elision** — `cast(dtype, x)` where `x` already has `dtype` is replaced by `x`.
  *   - **Canonical commutative ordering** — for `Add`, `Mul`, `Min`, `Max`, `Eq`, `Neq`, `And`, `Or`, operands are
  *     stored in ascending `NodeId` order so that `x + y` and `y + x` produce the same node after hash-consing.
  *   - **Dead-node pruning** — nodes not reachable from `output` are dropped.
  *
  * **NaN note:** `x * 0 → 0` is applied even when `x` may be NaN or infinite at runtime, which is not strictly IEEE-754
  * compliant. If strict NaN propagation is required, guard this rewrite at the call site.
  *
  * The pass is **idempotent**: `run(run(g))` produces a graph that is structurally equal to `run(g)`.
  */
object Normalize:

  private val CommutativeOps: Set[BinaryOp] =
    Set(BinaryOp.Add, BinaryOp.Mul, BinaryOp.Min, BinaryOp.Max, BinaryOp.Eq, BinaryOp.Neq, BinaryOp.And, BinaryOp.Or)

  /** Run all normalisation rules on `graph` and return the simplified graph.
    *
    * The returned graph has a fresh dense `NodeId` space: dead nodes are removed and survivors are re-indexed starting
    * from 0.
    */
  def run(graph: TensorGraph): TensorGraph =

    // ── 1. Collect live nodes in topological order (post-order DFS) ────────
    val visited = new Array[Boolean](graph.size)
    val topoOrder = mutable.ArrayBuffer[NodeId]()

    def dfs(id: NodeId): Unit =
      if !visited(id.i) then
        visited(id.i) = true
        graph(id) match
          case TensorExpr.Unary(_, a, _)     => dfs(a)
          case TensorExpr.Binary(_, a, b, _) => dfs(a); dfs(b)
          case TensorExpr.Cast(_, a, _)      => dfs(a)
          case TensorExpr.BCast(a, _, _)     => dfs(a)
          case TensorExpr.Reduce(_, a, _, _) => dfs(a)
          case TensorExpr.Where(c, x, y, _)  => dfs(c); dfs(x); dfs(y)
          case _                             => () // Const, Param, Lift — leaves
        end match
        topoOrder += id
    end dfs
    dfs(graph.output)

    // ── 2. Rebuild with hash-consing, applying rewrites ────────────────────
    val newNodes = mutable.ArrayBuffer[TensorExpr]()
    val indexMap = mutable.HashMap[TensorExpr, NodeId]()
    val remap = new Array[NodeId](graph.size) // old NodeId → new NodeId

    def intern(node: TensorExpr): NodeId =
      indexMap.getOrElse(
        node, {
          val id = NodeId(newNodes.size)
          newNodes += node
          indexMap(node) = id
          id
        }
      )
    end intern

    // Query helpers — operate on the *new* (rebuilt) node buffer via remapped ids
    def newNode(newId: NodeId): TensorExpr = newNodes(newId.i)

    def f64Val(newId: NodeId): Option[Double] = newNode(newId) match
      case TensorExpr.Const(v: Double, TType(DType.F64, s)) if s.isScalar => Some(v)
      case _                                                              => None

    def boolVal(newId: NodeId): Option[Boolean] = newNode(newId) match
      case TensorExpr.Const(v: Boolean, TType(DType.Bool, s)) if s.isScalar => Some(v)
      case _                                                                => None

    def isF64Zero(newId: NodeId): Boolean = f64Val(newId).contains(0.0)
    def isF64One(newId: NodeId): Boolean = f64Val(newId).contains(1.0)

    // ── Constant folding for Unary (F64 or Bool) ───────────────────────────
    def foldUnary(op: UnaryOp, na: NodeId, tpe: TType): Option[NodeId] =
      f64Val(na)
        .flatMap { v =>
          import UnaryOp.*
          val r: Option[Double] = op match
            case Neg        => Some(-v)
            case Sin        => Some(math.sin(v))
            case Cos        => Some(math.cos(v))
            case Tan        => Some(math.tan(v))
            case Exp        => Some(math.exp(v))
            case Log        => Some(math.log(v))
            case Sqrt       => Some(math.sqrt(v))
            case Abs        => Some(math.abs(v))
            case Reciprocal => Some(1.0 / v)
            case Not        => None
          r.map(d => intern(TensorExpr.Const(d, tpe)))
        }
        .orElse(
          boolVal(na).flatMap { v =>
            if op == UnaryOp.Not then Some(intern(TensorExpr.Const(!v, tpe))) else None
          }
        )
    end foldUnary

    // ── Constant folding for Binary ────────────────────────────────────────
    def foldBinary(op: BinaryOp, na: NodeId, nb: NodeId, tpe: TType): Option[NodeId] =
      // F64 numeric result
      f64Val(na)
        .zip(f64Val(nb))
        .flatMap { case (va, vb) =>
          import BinaryOp.*
          val numR: Option[Double] = op match
            case Add => Some(va + vb)
            case Sub => Some(va - vb)
            case Mul => Some(va * vb)
            case Div => Some(va / vb)
            case Pow => Some(math.pow(va, vb))
            case Min => Some(math.min(va, vb))
            case Max => Some(math.max(va, vb))
            case _   => None
          val cmpR: Option[Boolean] = op match
            case Eq  => Some(va == vb)
            case Neq => Some(va != vb)
            case Lt  => Some(va < vb)
            case Lte => Some(va <= vb)
            case Gt  => Some(va > vb)
            case Gte => Some(va >= vb)
            case _   => None
          numR
            .map(d => intern(TensorExpr.Const(d, tpe)))
            .orElse(cmpR.map(b => intern(TensorExpr.Const(b, tpe))))
        }
        .orElse(
          // Bool × Bool result
          boolVal(na).zip(boolVal(nb)).flatMap { case (va, vb) =>
            import BinaryOp.*
            val r: Option[Boolean] = op match
              case And => Some(va && vb)
              case Or  => Some(va || vb)
              case Eq  => Some(va == vb)
              case Neq => Some(va != vb)
              case _   => None
            r.map(b => intern(TensorExpr.Const(b, tpe)))
          }
        )
    end foldBinary

    // ── Identity / annihilator rewrites (F64 only) ────────────────────────
    def simplifyBinary(op: BinaryOp, na: NodeId, nb: NodeId, tpe: TType): Option[NodeId] =
      import BinaryOp.*
      op match
        // x + 0 → x, 0 + x → x
        case Add if isF64Zero(nb) => Some(na)
        case Add if isF64Zero(na) => Some(nb)
        // x - 0 → x
        case Sub if isF64Zero(nb) => Some(na)
        // x * 1 → x, 1 * x → x
        case Mul if isF64One(nb) => Some(na)
        case Mul if isF64One(na) => Some(nb)
        // x * 0 → 0, 0 * x → 0
        case Mul if isF64Zero(nb) => Some(intern(TensorExpr.Const(0.0, tpe)))
        case Mul if isF64Zero(na) => Some(intern(TensorExpr.Const(0.0, tpe)))
        // x / 1 → x
        case Div if isF64One(nb) => Some(na)
        // 0 / x → 0  (only when x is a known non-zero F64 constant, to preserve div-by-zero semantics)
        case Div if isF64Zero(na) && f64Val(nb).exists(_ != 0.0) => Some(intern(TensorExpr.Const(0.0, tpe)))
        // pow(x, 1) → x
        case Pow if isF64One(nb) => Some(na)
        // pow(x, 0) → 1
        case Pow if isF64Zero(nb) => Some(intern(TensorExpr.Const(1.0, tpe)))
        // pow(1, x) → 1
        case Pow if isF64One(na) => Some(intern(TensorExpr.Const(1.0, tpe)))
        case _                   => None
      end match
    end simplifyBinary

    // ── Main walk ──────────────────────────────────────────────────────────
    def processNode(node: TensorExpr): NodeId =
      node match
        case c: TensorExpr.Const => intern(c)
        case p: TensorExpr.Param => intern(p)
        case l: TensorExpr.Lift  => intern(l)

        case TensorExpr.Unary(op, a, tpe) =>
          val na = remap(a.i)
          foldUnary(op, na, tpe).getOrElse(intern(TensorExpr.Unary(op, na, tpe)))

        case TensorExpr.Binary(op, a, b, tpe) =>
          val na = remap(a.i)
          val nb = remap(b.i)
          foldBinary(op, na, nb, tpe)
            .orElse(simplifyBinary(op, na, nb, tpe))
            .getOrElse {
              // Canonical commutative ordering: smaller NodeId first
              val (ca, cb) =
                if CommutativeOps.contains(op) && na.i > nb.i then (nb, na) else (na, nb)
              intern(TensorExpr.Binary(op, ca, cb, tpe))
            }

        case TensorExpr.Cast(to, a, tpe) =>
          val na = remap(a.i)
          if newNode(na).tpe.dtype == to then na
          else intern(TensorExpr.Cast(to, na, tpe))
          end if

        case TensorExpr.BCast(a, to, tpe) =>
          intern(TensorExpr.BCast(remap(a.i), to, tpe))

        case TensorExpr.Reduce(op, a, axes, tpe) =>
          intern(TensorExpr.Reduce(op, remap(a.i), axes, tpe))

        case TensorExpr.Where(c, x, y, tpe) =>
          val nc = remap(c.i)
          val nx = remap(x.i)
          val ny = remap(y.i)
          boolVal(nc)
            .map(flag => if flag then nx else ny)
            .getOrElse(intern(TensorExpr.Where(nc, nx, ny, tpe)))
      end match
    end processNode

    for oldId <- topoOrder do remap(oldId.i) = processNode(graph(oldId))
    end for

    // ── 3. Compact: prune nodes that became dead after simplification ───────
    // Simplification can make previously-live nodes unreachable (e.g., Const(0)
    // after x+0→x). A second DFS on the rebuilt buffer ensures the final graph
    // has no dead nodes — which is required for idempotence.
    val rawOutput = remap(graph.output.i)
    val live2 = new Array[Boolean](newNodes.size)
    val liveOrder = mutable.ArrayBuffer[NodeId]()

    def dfs2(id: NodeId): Unit =
      if !live2(id.i) then
        live2(id.i) = true
        newNodes(id.i) match
          case TensorExpr.Unary(_, a, _)     => dfs2(a)
          case TensorExpr.Binary(_, a, b, _) => dfs2(a); dfs2(b)
          case TensorExpr.Cast(_, a, _)      => dfs2(a)
          case TensorExpr.BCast(a, _, _)     => dfs2(a)
          case TensorExpr.Reduce(_, a, _, _) => dfs2(a)
          case TensorExpr.Where(c, x, y, _)  => dfs2(c); dfs2(x); dfs2(y)
          case _                             => ()
        end match
        liveOrder += id
    end dfs2
    dfs2(rawOutput)

    val compactNodes = mutable.ArrayBuffer[TensorExpr]()
    val compactRemap = new Array[NodeId](newNodes.size)
    for id <- liveOrder do
      val newId = NodeId(compactNodes.size)
      compactNodes += rewriteChildIds(newNodes(id.i), compactRemap)
      compactRemap(id.i) = newId
    end for

    TensorGraph(compactNodes.toVector, compactRemap(rawOutput.i))
  end run

  /** Rewrite child `NodeId` references in a node using the given mapping. */
  private def rewriteChildIds(node: TensorExpr, m: Array[NodeId]): TensorExpr =
    node match
      case TensorExpr.Unary(op, a, tpe)      => TensorExpr.Unary(op, m(a.i), tpe)
      case TensorExpr.Binary(op, a, b, tpe)  => TensorExpr.Binary(op, m(a.i), m(b.i), tpe)
      case TensorExpr.Cast(to, a, tpe)       => TensorExpr.Cast(to, m(a.i), tpe)
      case TensorExpr.BCast(a, to, tpe)      => TensorExpr.BCast(m(a.i), to, tpe)
      case TensorExpr.Reduce(op, a, ax, tpe) => TensorExpr.Reduce(op, m(a.i), ax, tpe)
      case TensorExpr.Where(c, x, y, tpe)    => TensorExpr.Where(m(c.i), m(x.i), m(y.i), tpe)
      case leaf                              => leaf

end Normalize
