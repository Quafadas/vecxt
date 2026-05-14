package vecxt.fusion

import munit.FunSuite

class FusionPlannerPhase6Test extends FunSuite:

  // ── Shared type aliases ───────────────────────────────────────────────────
  val f64s  = TType(DType.F64, Shape.scalar)
  val bools = TType(DType.Bool, Shape.scalar)
  val f64v4 = TType(DType.F64, Shape(Dim.Known(4)))
  val f64m  = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))

  // ── Graph-builder helpers ─────────────────────────────────────────────────

  def param(name: String, tpe: TType = f64v4): TensorExpr = TensorExpr.Param(name, tpe)
  def c(v: Double, tpe: TType = f64s): TensorExpr         = TensorExpr.Const(v, tpe)
  def g(nodes: TensorExpr*): TensorGraph                   = TensorGraph(nodes.toVector, NodeId(nodes.length - 1))

  // ══════════════════════════════════════════════════════════════════════════
  // Group count and basic structure
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: single Param leaf → no groups") {
    val graph = g(param("x"))
    val plan  = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 0)
    assertEquals(plan.assignment.size, 0)
  }

  test("fuse: single Unary(Sin) → one group") {
    // 0=Param(x), 1=Sin(0)
    val graph = g(param("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4))
    val plan  = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds, Vector(NodeId(1)))
  }

  test("fuse: chain sin(cos(x)) → one group with two nodes") {
    // 0=Param, 1=Cos(0), 2=Sin(1)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(1), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds.size, 2)
    assertEquals(plan.groups(0).output, NodeId(2))
  }

  test("fuse: sin(x) + cos(x) → one group, three nodes, one input") {
    // 0=Param(x), 1=Sin(0), 2=Cos(0), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds.size, 3)
    assertEquals(plan.groups(0).inputs, Vector(NodeId(0)))
    assertEquals(plan.groups(0).output, NodeId(3))
  }

  test("fuse: sin(x) + cos(y) → one group, three nodes, two inputs") {
    // 0=Param(x), 1=Param(y), 2=Sin(0), 3=Cos(1), 4=Add(2,3)
    val graph = g(
      param("x"),
      param("y"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(2), NodeId(3), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).inputs.toSet, Set(NodeId(0), NodeId(1)))
  }

  test("fuse: terminal reduce sum(sin(x)+1) → one group with embedded reduce") {
    // 0=Param(x), 1=Const(1), 2=Sin(0), 3=Add(2,1), 4=Reduce(Sum,3)
    val graph = g(
      param("x"),
      c(1.0, f64v4),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(2), NodeId(1), f64v4),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(3), Vector(0), f64s)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    // Group contains Sin, Add, Reduce (3 non-leaf nodes)
    assertEquals(plan.groups(0).nodeIds.size, 3)
    // Reduce is the output (last node)
    assertEquals(plan.groups(0).output, NodeId(4))
  }

  test("fuse: multi-consumer y = sin(x); exp(y) + y → two groups, y is barrier") {
    // 0=Param(x), 1=Sin(0)=y, 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    // y=sin(x) has two consumers → barrier; forces two groups
    assertEquals(plan.groups.size, 2)
  }

  test("fuse: two independent reduces → three groups") {
    // 0=Param(x), 1=Param(y), 2=Reduce(Sum,0), 3=Reduce(Sum,1), 4=Add(2,3)
    val graph = g(
      param("x"),
      param("y"),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64s),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(1), Vector(0), f64s),
      TensorExpr.Binary(BinaryOp.Add, NodeId(2), NodeId(3), f64s)
    )
    val plan = FusionPlanner.plan(graph)
    // Reduce(sum,x), Reduce(sum,y) in separate groups; Add in third group
    assertEquals(plan.groups.size, 3)
  }

  test("fuse: standalone reduce → singleton reduce group") {
    // 0=Param(x), 1=Reduce(Sum,0)
    val graph = g(param("x"), TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64s))
    val plan  = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds, Vector(NodeId(1)))
    assertEquals(plan.groups(0).inputs, Vector(NodeId(0)))
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Partition properties
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: partition — every non-leaf node has exactly one assignment") {
    // 0=Param(x), 1=Sin(0), 2=Cos(0), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan     = FusionPlanner.plan(graph)
    val nonLeafs = Set(NodeId(1), NodeId(2), NodeId(3))
    assertEquals(plan.assignment.keySet, nonLeafs)
  }

  test("fuse: partition — leaf nodes not in assignment") {
    val graph = g(
      param("x"),
      param("y"),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assert(!plan.assignment.contains(NodeId(0)))
    assert(!plan.assignment.contains(NodeId(1)))
    assert(plan.assignment.contains(NodeId(2)))
  }

  test("fuse: partition — no node appears in two groups") {
    // 0=Param(x), 1=Sin(0)=y, 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan   = FusionPlanner.plan(graph)
    val allIds = plan.groups.flatMap(_.nodeIds)
    assertEquals(allIds.size, allIds.distinct.size)
  }

  test("fuse: partition — union of group nodeIds = all non-leaf NodeIds") {
    // 0=Param(x), 1=Sin(0)=y, 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan       = FusionPlanner.plan(graph)
    val fromGroups = plan.groups.flatMap(_.nodeIds).toSet
    val fromAssign = plan.assignment.keySet
    assertEquals(fromGroups, fromAssign)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Execution order
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: groups are in ascending output NodeId order (execution order)") {
    // 0=Param(x), 1=Sin(0)=y, 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan    = FusionPlanner.plan(graph)
    val outputs = plan.groups.map(_.output.i)
    assertEquals(outputs, outputs.sorted)
  }

  test("fuse: producer group executes before consumer group") {
    // 0=Param(x), 1=Sin(0)=y, 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    // The group containing sin(x) (output=1) must come before the group with Add (output=3)
    val sinGid = plan.groupOf(NodeId(1)).get
    val addGid = plan.groupOf(NodeId(3)).get
    assert(sinGid.i < addGid.i)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // FusionGroup structural invariants
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: group.output == group.nodeIds.last") {
    // 0=Param(x), 1=Sin(0), 2=Cos(0), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    plan.groups.foreach(grp => assertEquals(grp.output, grp.nodeIds.last))
  }

  test("fuse: group.nodeIds are in ascending order") {
    // 0=Param(x), 1=Sin(0), 2=Cos(0), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    plan.groups.foreach { grp =>
      val ids = grp.nodeIds.map(_.i)
      assertEquals(ids, ids.sorted)
    }
  }

  test("fuse: group.inputs don't overlap with group.nodeIds") {
    // 0=Param(x), 1=Sin(0)=y, 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    plan.groups.foreach { grp =>
      val nodeSet  = grp.nodeIds.toSet
      val inputSet = grp.inputs.toSet
      assert(nodeSet.intersect(inputSet).isEmpty, s"group ${grp.output} has overlap")
    }
  }

  test("fuse: group.output is the graph output for the final group") {
    // 0=Param(x), 1=Sin(0), 2=Cos(0), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.last.output, graph.output)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // FusionPlan.groupOf
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: groupOf returns None for a leaf") {
    val graph = g(param("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4))
    val plan  = FusionPlanner.plan(graph)
    assertEquals(plan.groupOf(NodeId(0)), None)
  }

  test("fuse: groupOf returns Some for a non-leaf") {
    val graph = g(param("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4))
    val plan  = FusionPlanner.plan(graph)
    assertEquals(plan.groupOf(NodeId(1)), Some(GroupId(0)))
  }

  test("fuse: groupOf multi-consumer case — barrier node in its own group") {
    // 0=Param(x), 1=Sin(0)=y [refCount=2], 2=Exp(1), 3=Add(1,2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    // sin(x) is in a separate group from exp+add
    val sinGid = plan.groupOf(NodeId(1)).get
    val expGid = plan.groupOf(NodeId(2)).get
    assertNotEquals(sinGid, expGid)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Elementwise node types: Cast, BCast, Where
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: Cast node is elementwise and fused with its producer") {
    // 0=Param(x:f64v4), 1=Sin(0), 2=Cast(Bool,1)
    val f64Bool = TType(DType.Bool, Shape(Dim.Known(4)))
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Cast(DType.Bool, NodeId(1), f64Bool)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds.size, 2)
  }

  test("fuse: BCast node is elementwise and fused with its producer") {
    // 0=Param(x:f64s), 1=BCast(0, [4])
    val graph = g(
      param("x", f64s),
      TensorExpr.BCast(NodeId(0), Shape(Dim.Known(4)), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds, Vector(NodeId(1)))
  }

  test("fuse: Where node is elementwise and fused with all three producers") {
    // 0=Param(cond:bool), 1=Param(x), 2=Param(y), 3=Where(0,1,2)
    val boolV4 = TType(DType.Bool, Shape(Dim.Known(4)))
    val graph = g(
      param("cond", boolV4),
      param("x"),
      param("y"),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds, Vector(NodeId(3)))
    assertEquals(plan.groups(0).inputs.toSet, Set(NodeId(0), NodeId(1), NodeId(2)))
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Reduce placement rules
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: elementwise-seeded group never absorbs a Reduce producer") {
    // 0=Param(x), 1=Reduce(Sum,0) [refCount=1], 2=Sin(1)
    // Sin seeds the group → gSeedIsElemwise=true → Reduce cannot be absorbed
    val graph = g(
      param("x"),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64s),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(1), f64s)
    )
    val plan = FusionPlanner.plan(graph)
    // Sin's group cannot absorb the Reduce; two groups expected
    assertEquals(plan.groups.size, 2)
    val sinGid    = plan.groupOf(NodeId(2)).get
    val reduceGid = plan.groupOf(NodeId(1)).get
    assertNotEquals(sinGid, reduceGid)
  }

  test("fuse: Reduce-seeded group does absorb its elementwise producers") {
    // 0=Param(x), 1=Sin(0), 2=Reduce(Sum,1)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(1), Vector(0), f64s)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).nodeIds.toSet, Set(NodeId(1), NodeId(2)))
  }

  test("fuse: two reduces on the same input are in separate groups") {
    // 0=Param(x), 1=Reduce(Sum,0), 2=Reduce(Product,0)  [both refCount=1 but x has refCount=2]
    // Result depends on whether x being multi-consumer forces separation
    // x is a Param (leaf), so it's fine. But the Add is elementwise-seeded.
    val graph = g(
      param("x"),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64s),
      TensorExpr.Reduce(ReduceOp.Product, NodeId(0), Vector(0), f64s),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64s)
    )
    val plan = FusionPlanner.plan(graph)
    // Reduce(Sum) and Reduce(Product) must be in different groups
    val sumGid = plan.groupOf(NodeId(1)).get
    val prdGid = plan.groupOf(NodeId(2)).get
    assertNotEquals(sumGid, prdGid)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // CostModel
  // ══════════════════════════════════════════════════════════════════════════

  test("costmodel: nodeFlops for Add (1-element) = 1") {
    val node = TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s)
    assertEquals(CostModel.nodeFlops(node), 1L)
  }

  test("costmodel: nodeFlops for Add (4-element) = 4") {
    val node = TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64v4)
    assertEquals(CostModel.nodeFlops(node), 4L)
  }

  test("costmodel: nodeFlops for Sin (1-element) = 20 (transcendental)") {
    val node = TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64s)
    assertEquals(CostModel.nodeFlops(node), 20L)
  }

  test("costmodel: nodeFlops for Neg (1-element) = 1 (cheap)") {
    val node = TensorExpr.Unary(UnaryOp.Neg, NodeId(0), f64s)
    assertEquals(CostModel.nodeFlops(node), 1L)
  }

  test("costmodel: nodeFlops for BCast = 0") {
    val node = TensorExpr.BCast(NodeId(0), Shape(Dim.Known(4)), f64v4)
    assertEquals(CostModel.nodeFlops(node), 0L)
  }

  test("costmodel: nodeFlops for Param = 0 (leaf)") {
    val node = TensorExpr.Param("x", f64v4)
    assertEquals(CostModel.nodeFlops(node), 0L)
  }

  test("costmodel: estimatedFlops for sin(x)+cos(x) group") {
    // f64v4 has 4 elements: Sin=4×20=80, Cos=4×20=80, Add=4×1=4 → total 164 FLOPs
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan  = FusionPlanner.plan(graph)
    val grp   = plan.groups(0)
    val flops = CostModel.estimatedFlops(grp, graph)
    assertEquals(flops, 164L)
  }

  test("costmodel: estimatedBytesRead for group with 1 input of size 4") {
    // Group: Unary(Sin, param), input = 1 × 4 elements × 8 bytes = 32
    val graph = g(param("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4))
    val plan  = FusionPlanner.plan(graph)
    val grp   = plan.groups(0)
    assertEquals(CostModel.estimatedBytesRead(grp, graph), 32L)
  }

  test("costmodel: estimatedBytesRead for group with 2 inputs of size 4") {
    // sin(x)+cos(x): 2 inputs but only 1 distinct leaf (x), however inputs list = [x]
    // Let's use sin(x)+cos(y): 2 distinct inputs
    val graph = g(
      param("x"),
      param("y"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(2), NodeId(3), f64v4)
    )
    val plan  = FusionPlanner.plan(graph)
    val grp   = plan.groups(0)
    // 2 inputs × 4 elements × 8 bytes = 64
    assertEquals(CostModel.estimatedBytesRead(grp, graph), 64L)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Integration: plan after normalisation
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: plan(normalize(g)) has same group count for simple chain") {
    // sin(x) + cos(x) normalizes to the same structure → still one group
    val raw = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val normalised = Normalize.run(raw)
    val plan       = FusionPlanner.plan(normalised)
    assertEquals(plan.groups.size, 1)
  }

  test("fuse: plan after normalisation removes dead nodes and re-plans cleanly") {
    // Scalar x + scalar 0 normalises to just Param(x) (Add and Const are pruned)
    val raw = g(
      param("x", f64s),
      c(0.0, f64s),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s)
    )
    val normalised = Normalize.run(raw)
    // After normalisation: only Param remains (Add and Const are pruned)
    assertEquals(normalised.size, 1)
    val plan = FusionPlanner.plan(normalised)
    assertEquals(plan.groups.size, 0)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Misc edge cases
  // ══════════════════════════════════════════════════════════════════════════

  test("fuse: Binary node with same operand on both sides — one input in group") {
    // 0=Param(x), 1=Add(0,0)  — x used twice
    val graph = g(
      param("x"),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(0), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).inputs, Vector(NodeId(0)))
  }

  test("fuse: three-op chain — single group, correct input and output") {
    // 0=Param(x), 1=Abs(0), 2=Sqrt(1), 3=Neg(2)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Abs, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Sqrt, NodeId(1), f64v4),
      TensorExpr.Unary(UnaryOp.Neg, NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(graph)
    assertEquals(plan.groups.size, 1)
    assertEquals(plan.groups(0).inputs, Vector(NodeId(0)))
    assertEquals(plan.groups(0).output, NodeId(3))
    assertEquals(plan.groups(0).nodeIds.size, 3)
  }

  test("fuse: FusionPlan holds a reference to the original graph") {
    val graph = g(param("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4))
    val plan  = FusionPlanner.plan(graph)
    assertEquals(plan.graph, graph)
  }

end FusionPlannerPhase6Test
