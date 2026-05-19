package vecxt.fusion

import scala.reflect.ClassTag
import vecxt.ndarray.NDArray

/** Internal result type for the Phase-7 reference interpreter.
  *
  * Each variant wraps an [[NDArray]] that is guaranteed to be contiguous col-major (F-order), matching vecxt's storage
  * convention.
  */
sealed trait IVal:
  def shape: Array[Int]

  final def numel: Int =
    var p = 1
    var i = 0
    while i < shape.length do
      p *= shape(i)
      i += 1
    p
  end numel

  final def rank: Int       = shape.length
  final def isScalar: Boolean = shape.isEmpty
end IVal

object IVal:
  /** F64 (Double) tensor. `nd` is always contiguous col-major. */
  final case class F64(nd: NDArray[Double]) extends IVal:
    def shape: Array[Int] = nd.shape

  /** I64 (Long) tensor. `nd` is always contiguous col-major. */
  final case class I64(nd: NDArray[Long]) extends IVal:
    def shape: Array[Int] = nd.shape

  /** Bool (Boolean) tensor. `nd` is always contiguous col-major. */
  final case class Bool(nd: NDArray[Boolean]) extends IVal:
    def shape: Array[Int] = nd.shape

  // ── NDArray → IVal converters ─────────────────────────────────────────────

  /** Wrap or materialise an [[NDArray]][Double] as [[IVal.F64]].
    *
    * If the source is already contiguous col-major it is used as-is; otherwise the data is copied into a fresh
    * col-major layout.
    */
  def fromNDArray(nd: NDArray[Double]): IVal.F64 =
    if nd.isColMajor then IVal.F64(nd)
    else
      val n   = nd.numel
      val out = new Array[Double](n)
      readInto(nd.data, nd.shape, nd.strides, nd.offset, false, out)
      val s = nd.shape.clone()
      IVal.F64(NDArray.wrap(out, s, cmStrides(s)))
  end fromNDArray

  /** Wrap or materialise an [[NDArray]][Boolean] as [[IVal.Bool]]. */
  def fromNDArrayBool(nd: NDArray[Boolean]): IVal.Bool =
    if nd.isColMajor then IVal.Bool(nd)
    else
      val n   = nd.numel
      val out = new Array[Boolean](n)
      readInto(nd.data, nd.shape, nd.strides, nd.offset, false, out)
      val s = nd.shape.clone()
      IVal.Bool(NDArray.wrap(out, s, cmStrides(s)))
  end fromNDArrayBool

  /** Wrap or materialise an [[NDArray]][Long] as [[IVal.I64]]. */
  def fromNDArrayLong(nd: NDArray[Long]): IVal.I64 =
    if nd.isColMajor then IVal.I64(nd)
    else
      val n   = nd.numel
      val out = new Array[Long](n)
      readInto(nd.data, nd.shape, nd.strides, nd.offset, false, out)
      val s = nd.shape.clone()
      IVal.I64(NDArray.wrap(out, s, cmStrides(s)))
  end fromNDArrayLong

  // ── package-private helpers ───────────────────────────────────────────────

  /** Column-major strides: stride(0)=1, stride(k) = d₀ × … × d(k−1). */
  private[fusion] def cmStrides(shape: Array[Int]): Array[Int] =
    if shape.isEmpty then Array.emptyIntArray
    else
      val s = new Array[Int](shape.length)
      s(0) = 1
      var k = 1
      while k < shape.length do
        s(k) = s(k - 1) * shape(k - 1)
        k += 1
      s
  end cmStrides

  /** Product of all shape elements; returns 1 for the empty (scalar) shape. */
  private[fusion] def shapeProduct(shape: Array[Int]): Int =
    var p = 1
    var i = 0
    while i < shape.length do
      p *= shape(i)
      i += 1
    p
  end shapeProduct

  /** Build a contiguous col-major [[IVal.F64]] from a flat data array and shape. */
  private[fusion] def f64cm(data: Array[Double], shape: Array[Int]): IVal.F64 =
    IVal.F64(NDArray.wrap(data, shape, cmStrides(shape)))

  /** Build a contiguous col-major [[IVal.I64]] from a flat data array and shape. */
  private[fusion] def i64cm(data: Array[Long], shape: Array[Int]): IVal.I64 =
    IVal.I64(NDArray.wrap(data, shape, cmStrides(shape)))

  /** Build a contiguous col-major [[IVal.Bool]] from a flat data array and shape. */
  private[fusion] def boolcm(data: Array[Boolean], shape: Array[Int]): IVal.Bool =
    IVal.Bool(NDArray.wrap(data, shape, cmStrides(shape)))

  // ── private copy helper ───────────────────────────────────────────────────

  /** Copy `n` logical elements from a strided source array into a flat output array. */
  private def readInto[A: ClassTag](
      src:        Array[A],
      shape:      Array[Int],
      physStride: Array[Int],
      offset:     Int,
      colMajor:   Boolean,
      out:        Array[A]
  ): Unit =
    val n    = out.length
    val ndim = shape.length
    if colMajor then System.arraycopy(src, offset, out, 0, n)
    else
      val sp = cmStrides(shape)
      var j  = 0
      while j < n do
        var pos = offset
        var k   = 0
        while k < ndim do
          val coord = (j / sp(k)) % shape(k)
          pos += coord * physStride(k)
          k += 1
        end while
        out(j) = src(pos)
        j += 1
      end while
  end readInto

end IVal

// ─────────────────────────────────────────────────────────────────────────────

/** Thrown by the interpreter when evaluation cannot proceed. */
final class InterpError(msg: String) extends RuntimeException(msg)

/** Phase-7 reference interpreter – correctness oracle, not on any hot path.
  *
  * Evaluates a `TensorGraph` by walking nodes in topological order (indices 0…n-1, guaranteed by construction). Each
  * node's result is stored in a local `results` array and referenced by later nodes.
  *
  * Supported dtypes: F64 → IVal.F64, I64 → IVal.I64, Bool → IVal.Bool. Other dtypes raise InterpError.
  */
object Interpreter:

  /** Evaluate `graph` and return the output node's value as an [[IVal]].
    *
    * @param graph
    *   The tensor graph to evaluate.
    * @param params
    *   Values for [[TensorExpr.Param]] nodes, keyed by name.
    * @param lifts
    *   Values for [[TensorExpr.Lift]] nodes, keyed by [[NDArrayHandle.id]].
    */
  def eval(
      graph:  TensorGraph,
      params: Map[String, IVal] = Map.empty,
      lifts:  Map[Int, IVal]   = Map.empty
  ): IVal =
    val results = new Array[IVal](graph.size)
    var i       = 0
    while i < graph.size do
      results(i) = step(graph(NodeId(i)), results, params, lifts)
      i += 1
    results(graph.output.i)
  end eval

  // ── node dispatch ─────────────────────────────────────────────────────────

  private def step(
      node:    TensorExpr,
      results: Array[IVal],
      params:  Map[String, IVal],
      lifts:   Map[Int, IVal]
  ): IVal = node match
    case TensorExpr.Const(value, tpe) =>
      tpe.dtype match
        case DType.F64  => IVal.f64cm(Array(value.asInstanceOf[Double]), Array.emptyIntArray)
        case DType.I64  => IVal.i64cm(Array(value.asInstanceOf[Long]), Array.emptyIntArray)
        case DType.Bool => IVal.boolcm(Array(value.asInstanceOf[Boolean]), Array.emptyIntArray)
        case dt         => throw InterpError(s"Const: unsupported dtype $dt")

    case TensorExpr.Param(name, _) =>
      params.getOrElse(name, throw InterpError(s"Missing param '$name'"))

    case TensorExpr.Lift(handle, _) =>
      lifts.getOrElse(handle.id, throw InterpError(s"Missing lift id ${handle.id}"))

    case TensorExpr.Unary(op, a, _) =>
      unary(op, results(a.i))

    case TensorExpr.Binary(op, a, b, _) =>
      binary(op, results(a.i), results(b.i))

    case TensorExpr.Cast(to, a, _) =>
      castVal(to, results(a.i))

    case TensorExpr.BCast(a, to, _) =>
      bcast(results(a.i), shapeToIntArray(to))

    case TensorExpr.Reduce(op, a, axes, _) =>
      if axes.isEmpty then results(a.i) // identity: no axes to reduce
      else reduce(op, results(a.i), axes)

    case TensorExpr.Where(c, x, y, _) =>
      where(results(c.i), results(x.i), results(y.i))

  end step

  // ── shape helpers ─────────────────────────────────────────────────────────

  private def shapeToIntArray(s: Shape): Array[Int] =
    val out = new Array[Int](s.rank)
    var i   = 0
    while i < s.rank do
      s.dims(i) match
        case Dim.Known(n) => out(i) = n
        case d            => throw InterpError(s"Cannot evaluate symbolic/unknown dim: $d")
      i += 1
    out
  end shapeToIntArray

  // ── unary ─────────────────────────────────────────────────────────────────

  private def unary(op: UnaryOp, a: IVal): IVal = a match
    case IVal.F64(nd) =>
      val data  = nd.data
      val shape = nd.shape
      val n     = data.length
      val out   = new Array[Double](n)
      var i     = 0
      op match
        case UnaryOp.Neg        => while i < n do { out(i) = -data(i); i += 1 }
        case UnaryOp.Sin        => while i < n do { out(i) = math.sin(data(i)); i += 1 }
        case UnaryOp.Cos        => while i < n do { out(i) = math.cos(data(i)); i += 1 }
        case UnaryOp.Tan        => while i < n do { out(i) = math.tan(data(i)); i += 1 }
        case UnaryOp.Exp        => while i < n do { out(i) = math.exp(data(i)); i += 1 }
        case UnaryOp.Log        => while i < n do { out(i) = math.log(data(i)); i += 1 }
        case UnaryOp.Sqrt       => while i < n do { out(i) = math.sqrt(data(i)); i += 1 }
        case UnaryOp.Abs        => while i < n do { out(i) = math.abs(data(i)); i += 1 }
        case UnaryOp.Reciprocal => while i < n do { out(i) = 1.0 / data(i); i += 1 }
        case UnaryOp.Not        => throw InterpError("UnaryOp.Not requires Bool input")
      IVal.f64cm(out, shape.clone())

    case IVal.Bool(nd) =>
      op match
        case UnaryOp.Not =>
          val data  = nd.data
          val n     = data.length
          val out   = new Array[Boolean](n)
          var i     = 0
          while i < n do { out(i) = !data(i); i += 1 }
          IVal.boolcm(out, nd.shape.clone())
        case _ => throw InterpError(s"Unary $op requires F64 input, got Bool")

    case IVal.I64(nd) =>
      val data  = nd.data
      val shape = nd.shape
      val n     = data.length
      val out   = new Array[Long](n)
      var i     = 0
      op match
        case UnaryOp.Neg => while i < n do { out(i) = -data(i); i += 1 }
        case UnaryOp.Abs => while i < n do { out(i) = math.abs(data(i)); i += 1 }
        case _           => throw InterpError(s"Unary $op not supported on I64")
      IVal.i64cm(out, shape.clone())

  end unary

  // ── binary (same-shape, elementwise) ─────────────────────────────────────

  private def binary(op: BinaryOp, a: IVal, b: IVal): IVal = (a, b) match
    case (IVal.F64(nda), IVal.F64(ndb)) =>
      val da = nda.data
      val db = ndb.data
      val sa = nda.shape
      val n  = da.length
      op match
        case BinaryOp.Eq | BinaryOp.Neq | BinaryOp.Lt | BinaryOp.Lte | BinaryOp.Gt | BinaryOp.Gte =>
          val out = new Array[Boolean](n)
          var i   = 0
          while i < n do
            out(i) = op match
              case BinaryOp.Eq  => da(i) == db(i)
              case BinaryOp.Neq => da(i) != db(i)
              case BinaryOp.Lt  => da(i) < db(i)
              case BinaryOp.Lte => da(i) <= db(i)
              case BinaryOp.Gt  => da(i) > db(i)
              case BinaryOp.Gte => da(i) >= db(i)
              case _            => false
            i += 1
          IVal.boolcm(out, sa.clone())
        case _ =>
          val out = new Array[Double](n)
          var i   = 0
          while i < n do
            out(i) = op match
              case BinaryOp.Add => da(i) + db(i)
              case BinaryOp.Sub => da(i) - db(i)
              case BinaryOp.Mul => da(i) * db(i)
              case BinaryOp.Div => da(i) / db(i)
              case BinaryOp.Pow => math.pow(da(i), db(i))
              case BinaryOp.Min => math.min(da(i), db(i))
              case BinaryOp.Max => math.max(da(i), db(i))
              case _            => throw InterpError(s"Binary $op not valid on F64")
            i += 1
          IVal.f64cm(out, sa.clone())

    case (IVal.Bool(nda), IVal.Bool(ndb)) =>
      val da  = nda.data
      val db  = ndb.data
      val n   = da.length
      val out = new Array[Boolean](n)
      var i   = 0
      while i < n do
        out(i) = op match
          case BinaryOp.And => da(i) && db(i)
          case BinaryOp.Or  => da(i) || db(i)
          case BinaryOp.Eq  => da(i) == db(i)
          case BinaryOp.Neq => da(i) != db(i)
          case _            => throw InterpError(s"Binary $op not valid on Bool")
        i += 1
      IVal.boolcm(out, nda.shape.clone())

    case (IVal.I64(nda), IVal.I64(ndb)) =>
      val da = nda.data
      val db = ndb.data
      val sa = nda.shape
      val n  = da.length
      op match
        case BinaryOp.Eq | BinaryOp.Neq | BinaryOp.Lt | BinaryOp.Lte | BinaryOp.Gt | BinaryOp.Gte =>
          val out = new Array[Boolean](n)
          var i   = 0
          while i < n do
            out(i) = op match
              case BinaryOp.Eq  => da(i) == db(i)
              case BinaryOp.Neq => da(i) != db(i)
              case BinaryOp.Lt  => da(i) < db(i)
              case BinaryOp.Lte => da(i) <= db(i)
              case BinaryOp.Gt  => da(i) > db(i)
              case BinaryOp.Gte => da(i) >= db(i)
              case _            => false
            i += 1
          IVal.boolcm(out, sa.clone())
        case _ =>
          val out = new Array[Long](n)
          var i   = 0
          while i < n do
            out(i) = op match
              case BinaryOp.Add => da(i) + db(i)
              case BinaryOp.Sub => da(i) - db(i)
              case BinaryOp.Mul => da(i) * db(i)
              case BinaryOp.Div => da(i) / db(i)
              case BinaryOp.Min => math.min(da(i), db(i))
              case BinaryOp.Max => math.max(da(i), db(i))
              case _            => throw InterpError(s"Binary $op not valid on I64")
            i += 1
          IVal.i64cm(out, sa.clone())

    case _ =>
      throw InterpError(
        s"Binary $op: incompatible types ${a.getClass.getSimpleName} vs ${b.getClass.getSimpleName}"
      )

  end binary

  // ── cast ──────────────────────────────────────────────────────────────────

  private def castVal(to: DType, a: IVal): IVal = (a, to) match
    case (_: IVal.F64, DType.F64)   => a
    case (_: IVal.I64, DType.I64)   => a
    case (_: IVal.Bool, DType.Bool) => a
    case (IVal.F64(nd), DType.I64) =>
      val d   = nd.data
      val out = new Array[Long](d.length)
      var i   = 0
      while i < d.length do { out(i) = d(i).toLong; i += 1 }
      IVal.i64cm(out, nd.shape.clone())
    case (IVal.F64(nd), DType.Bool) =>
      val d   = nd.data
      val out = new Array[Boolean](d.length)
      var i   = 0
      while i < d.length do { out(i) = d(i) != 0.0; i += 1 }
      IVal.boolcm(out, nd.shape.clone())
    case (IVal.I64(nd), DType.F64) =>
      val d   = nd.data
      val out = new Array[Double](d.length)
      var i   = 0
      while i < d.length do { out(i) = d(i).toDouble; i += 1 }
      IVal.f64cm(out, nd.shape.clone())
    case (IVal.I64(nd), DType.Bool) =>
      val d   = nd.data
      val out = new Array[Boolean](d.length)
      var i   = 0
      while i < d.length do { out(i) = d(i) != 0L; i += 1 }
      IVal.boolcm(out, nd.shape.clone())
    case (IVal.Bool(nd), DType.F64) =>
      val d   = nd.data
      val out = new Array[Double](d.length)
      var i   = 0
      while i < d.length do { out(i) = if d(i) then 1.0 else 0.0; i += 1 }
      IVal.f64cm(out, nd.shape.clone())
    case (IVal.Bool(nd), DType.I64) =>
      val d   = nd.data
      val out = new Array[Long](d.length)
      var i   = 0
      while i < d.length do { out(i) = if d(i) then 1L else 0L; i += 1 }
      IVal.i64cm(out, nd.shape.clone())
    case _ =>
      throw InterpError(s"Unsupported cast from ${a.getClass.getSimpleName} to $to")
  end castVal

  // ── explicit broadcast ────────────────────────────────────────────────────

  private def bcast(a: IVal, tgt: Array[Int]): IVal =
    val sp = IVal.cmStrides(tgt)
    val n  = IVal.shapeProduct(tgt)
    a match
      case IVal.F64(nd)  => IVal.f64cm(materialise(nd.data, nd.shape, tgt, sp, n), tgt.clone())
      case IVal.I64(nd)  => IVal.i64cm(materialise(nd.data, nd.shape, tgt, sp, n), tgt.clone())
      case IVal.Bool(nd) => IVal.boolcm(materialise(nd.data, nd.shape, tgt, sp, n), tgt.clone())
  end bcast

  /** Materialise a NumPy-style broadcast from `srcShape` to `tgtShape` (right-aligned, pad 1 on left). */
  private def materialise[A: ClassTag](
      srcData:    Array[A],
      srcShape:   Array[Int],
      tgtShape:   Array[Int],
      tgtStrides: Array[Int],
      n:          Int
  ): Array[A] =
    val out     = new Array[A](n)
    val tgtRank = tgtShape.length
    val srcRank = srcShape.length
    val srcSt   = IVal.cmStrides(srcShape)
    var flat    = 0
    while flat < n do
      var srcFlat = 0
      var d       = 0
      while d < tgtRank do
        val coord = (flat / tgtStrides(d)) % tgtShape(d)
        val srcD  = d - (tgtRank - srcRank) // corresponding src dim (negative → padded)
        if srcD >= 0 then
          val srcCoord = if srcShape(srcD) == 1 then 0 else coord
          srcFlat += srcCoord * srcSt(srcD)
        end if
        d += 1
      end while
      out(flat) = srcData(srcFlat)
      flat += 1
    out
  end materialise

  // ── reduce ────────────────────────────────────────────────────────────────

  private def reduce(op: ReduceOp, a: IVal, axes: Vector[Int]): IVal =
    // Sort descending so each removal doesn't shift remaining axis indices
    val sortedAxes = axes.sorted.reverse.toArray
    a match
      case v: IVal.F64  => reduceF64(op, v, sortedAxes)
      case v: IVal.Bool => reduceBool(op, v, sortedAxes)
      case v: IVal.I64  => reduceI64(op, v, sortedAxes)
  end reduce

  private def reduceF64(op: ReduceOp, a: IVal.F64, sortedAxes: Array[Int]): IVal =
    op match
      case ReduceOp.ArgMax | ReduceOp.ArgMin =>
        if sortedAxes.length != 1 then
          throw InterpError(s"ArgMax/ArgMin requires exactly one axis, got ${sortedAxes.length}")
        val (d, s) = argmaxAxisF64(a.nd.data, a.nd.shape, sortedAxes(0), ascending = op == ReduceOp.ArgMax)
        IVal.i64cm(d, s)
      case _ =>
        val identity: Double = op match
          case ReduceOp.Sum     => 0.0
          case ReduceOp.Product => 1.0
          case ReduceOp.Min     => Double.PositiveInfinity
          case ReduceOp.Max     => Double.NegativeInfinity
          case _                => throw InterpError(s"ReduceOp $op not valid on F64")
        val combine: (Double, Double) => Double = op match
          case ReduceOp.Sum     => (x, y) => x + y
          case ReduceOp.Product => (x, y) => x * y
          case ReduceOp.Min     => (x, y) => math.min(x, y)
          case ReduceOp.Max     => (x, y) => math.max(x, y)
          case _                => throw InterpError(s"ReduceOp $op not valid on F64")
        var data  = a.nd.data
        var shape = a.nd.shape
        var ai    = 0
        while ai < sortedAxes.length do
          val (d, s) = reduceAxis(data, shape, sortedAxes(ai), identity, combine)
          data = d; shape = s
          ai += 1
        IVal.f64cm(data, shape)
  end reduceF64

  private def reduceBool(op: ReduceOp, a: IVal.Bool, sortedAxes: Array[Int]): IVal =
    val identity: Boolean = op match
      case ReduceOp.All => true
      case ReduceOp.Any => false
      case _            => throw InterpError(s"ReduceOp $op not valid on Bool")
    val combine: (Boolean, Boolean) => Boolean = op match
      case ReduceOp.All => (x, y) => x && y
      case ReduceOp.Any => (x, y) => x || y
      case _            => throw InterpError(s"ReduceOp $op not valid on Bool")
    var data  = a.nd.data
    var shape = a.nd.shape
    var ai    = 0
    while ai < sortedAxes.length do
      val (d, s) = reduceAxis(data, shape, sortedAxes(ai), identity, combine)
      data = d; shape = s
      ai += 1
    IVal.boolcm(data, shape)
  end reduceBool

  private def reduceI64(op: ReduceOp, a: IVal.I64, sortedAxes: Array[Int]): IVal =
    val identity: Long = op match
      case ReduceOp.Sum     => 0L
      case ReduceOp.Product => 1L
      case ReduceOp.Min     => Long.MaxValue
      case ReduceOp.Max     => Long.MinValue
      case _                => throw InterpError(s"ReduceOp $op not valid on I64")
    val combine: (Long, Long) => Long = op match
      case ReduceOp.Sum     => (x, y) => x + y
      case ReduceOp.Product => (x, y) => x * y
      case ReduceOp.Min     => (x, y) => math.min(x, y)
      case ReduceOp.Max     => (x, y) => math.max(x, y)
      case _                => throw InterpError(s"ReduceOp $op not valid on I64")
    var data  = a.nd.data
    var shape = a.nd.shape
    var ai    = 0
    while ai < sortedAxes.length do
      val (d, s) = reduceAxis(data, shape, sortedAxes(ai), identity, combine)
      data = d; shape = s
      ai += 1
    IVal.i64cm(data, shape)
  end reduceI64

  /** Generic single-axis reduction (works for Double, Boolean, Long via type class). */
  private def reduceAxis[A: ClassTag](
      data:     Array[A],
      shape:    Array[Int],
      ax:       Int,
      identity: A,
      combine:  (A, A) => A
  ): (Array[A], Array[Int]) =
    val ndim     = shape.length
    val newShape = Array.tabulate(ndim - 1)(i => if i < ax then shape(i) else shape(i + 1))
    val newN     = IVal.shapeProduct(newShape)
    val out      = Array.fill(newN)(identity)
    val sp       = IVal.cmStrides(shape)
    val nsp      = IVal.cmStrides(newShape)
    val total    = data.length
    var flat     = 0
    while flat < total do
      var outFlat = 0
      var d       = 0
      while d < ndim do
        val coord = (flat / sp(d)) % shape(d)
        if d != ax then
          val nd2 = if d < ax then d else d - 1
          outFlat += coord * nsp(nd2)
        end if
        d += 1
      end while
      out(outFlat) = combine(out(outFlat), data(flat))
      flat += 1
    (out, newShape)
  end reduceAxis

  /** ArgMax or ArgMin along a single axis; returns (indices: Array[Long], newShape). */
  private def argmaxAxisF64(
      data:      Array[Double],
      shape:     Array[Int],
      ax:        Int,
      ascending: Boolean
  ): (Array[Long], Array[Int]) =
    val ndim     = shape.length
    val newShape = Array.tabulate(ndim - 1)(i => if i < ax then shape(i) else shape(i + 1))
    val newN     = IVal.shapeProduct(newShape)
    val sentinel = if ascending then Double.NegativeInfinity else Double.PositiveInfinity
    val bestVal  = Array.fill(newN)(sentinel)
    val bestIdx  = new Array[Long](newN)
    val sp       = IVal.cmStrides(shape)
    val nsp      = IVal.cmStrides(newShape)
    val total    = data.length
    var flat     = 0
    while flat < total do
      var outFlat = 0
      var axCoord = 0
      var d       = 0
      while d < ndim do
        val coord = (flat / sp(d)) % shape(d)
        if d == ax then axCoord = coord
        else
          val nd2 = if d < ax then d else d - 1
          outFlat += coord * nsp(nd2)
        end if
        d += 1
      end while
      val v = data(flat)
      if (ascending && v > bestVal(outFlat)) || (!ascending && v < bestVal(outFlat)) then
        bestVal(outFlat) = v
        bestIdx(outFlat) = axCoord.toLong
      end if
      flat += 1
    (bestIdx, newShape)
  end argmaxAxisF64

  // ── where ─────────────────────────────────────────────────────────────────

  private def where(cond: IVal, x: IVal, y: IVal): IVal =
    val condData = cond match
      case IVal.Bool(nd) => nd.data
      case _             => throw InterpError(s"Where: condition must be Bool, got ${cond.getClass.getSimpleName}")
    (x, y) match
      case (IVal.F64(ndx), IVal.F64(ndy)) =>
        val dx  = ndx.data
        val dy  = ndy.data
        val n   = dx.length
        val out = new Array[Double](n)
        var i   = 0
        while i < n do { out(i) = if condData(i) then dx(i) else dy(i); i += 1 }
        IVal.f64cm(out, ndx.shape.clone())
      case (IVal.I64(ndx), IVal.I64(ndy)) =>
        val dx  = ndx.data
        val dy  = ndy.data
        val n   = dx.length
        val out = new Array[Long](n)
        var i   = 0
        while i < n do { out(i) = if condData(i) then dx(i) else dy(i); i += 1 }
        IVal.i64cm(out, ndx.shape.clone())
      case (IVal.Bool(ndx), IVal.Bool(ndy)) =>
        val dx  = ndx.data
        val dy  = ndy.data
        val n   = dx.length
        val out = new Array[Boolean](n)
        var i   = 0
        while i < n do { out(i) = if condData(i) then dx(i) else dy(i); i += 1 }
        IVal.boolcm(out, ndx.shape.clone())
      case _ =>
        throw InterpError(
          s"Where: incompatible x/y types: ${x.getClass.getSimpleName} vs ${y.getClass.getSimpleName}"
        )
  end where

end Interpreter

