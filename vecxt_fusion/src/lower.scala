package vecxt.fusion

import scala.collection.mutable

/** Maps symbol names to their tensor type (dtype + shape). */
type TypeEnv = Map[String, TType]

/** Error produced while lowering a `MathExpr` to a `TensorGraph`. */
sealed trait LowerError:
  def message: String
end LowerError

object LowerError:

  /** A `Symbol` (or `Constant`) name was not found in the `TypeEnv`. */
  final case class UnknownSymbol(name: String) extends LowerError:
    def message: String = s"Unknown symbol: '$name'"
  end UnknownSymbol

  /** A binder form (`Sum`, `Integral`, `Subscript`, `Over`, `Under`, `SubSup`) has no TensorExpr representation yet.
    */
  final case class UnsupportedBinder(form: String) extends LowerError:
    def message: String = s"Unsupported binder: $form"
  end UnsupportedBinder

  /** `Superscript(base, exp)` where `exp` is not a literal `Number`. */
  final case class NonSemanticSuperscript(expRepr: String) extends LowerError:
    def message: String =
      s"Superscript exponent must be a literal Number, got: $expRepr"
  end NonSemanticSuperscript

  /** An unsupported or unknown `FunctionCall` name, or the wrong number of arguments. */
  final case class UnknownFunction(name: String, arity: Int) extends LowerError:
    def message: String = s"Unknown function or wrong arity: $name/$arity"
  end UnknownFunction

  /** A leaf or structural node that cannot be lowered to a `TensorExpr`. */
  final case class UnsupportedNode(repr: String) extends LowerError:
    def message: String = s"Cannot lower node: $repr"
  end UnsupportedNode

end LowerError

/** Phase-4 lowering: translates a `MathExpr[Double]` into a `TensorGraph`.
  *
  * The lowering is structural (AST-shape): it transforms `MathExpr` constructors one-to-one into `TensorExpr` nodes.
  * No constant folding, no normalisation, and no implicit broadcasting are performed here.
  *
  * Type information for `Param` nodes comes from `env`. Result types for all other nodes are derived from their child
  * nodes (the left child's type is used for binary arithmetic; TypeCheck can verify correctness afterwards).
  *
  * Hash-consing is applied: structurally identical nodes share a single `NodeId`, so `Symbol("x")` referenced twice in
  * the same expression produces one `Param` node.
  */
object Lower:

  /** Lower `expr` to a `TensorGraph`, resolving `Symbol`/`Constant` names via `env`.
    *
    * Fails immediately (fail-fast) on the first error encountered.
    */
  def lower(expr: MathExpr[Double], env: TypeEnv): Either[LowerError, TensorGraph] =
    val nodes = mutable.ArrayBuffer[TensorExpr]()
    val index = mutable.HashMap[TensorExpr, NodeId]()

    def intern(node: TensorExpr): NodeId =
      index.getOrElse(
        node, {
          val id = NodeId(nodes.size)
          nodes += node
          index(node) = id
          id
        }
      )
    end intern

    def tpeOf(id: NodeId): TType = nodes(id.i).tpe

    def binaryArith(op: BinaryOp, lhs: MathExpr[Double], rhs: MathExpr[Double]): Either[LowerError, NodeId] =
      for
        a <- go(lhs)
        b <- go(rhs)
      yield intern(TensorExpr.Binary(op, a, b, tpeOf(a)))
    end binaryArith

    def go(e: MathExpr[Double]): Either[LowerError, NodeId] =
      import MathExpr.*
      e match

        // ── Atoms ──────────────────────────────────────────────────────────────

        case Number(v) =>
          Right(intern(TensorExpr.Const(v, TType(DType.F64, Shape.scalar))))

        case Symbol(name) =>
          env.get(name) match
            case Some(tpe) => Right(intern(TensorExpr.Param(name, tpe)))
            case None      => Left(LowerError.UnknownSymbol(name))

        // Named mathematical constants (e.g. "pi", "e") are resolved from `env`
        // like any other symbol; the interpreter is expected to bind them.
        case Constant(name) =>
          env.get(name) match
            case Some(tpe) => Right(intern(TensorExpr.Param(name, tpe)))
            case None      => Left(LowerError.UnknownSymbol(name))

        // ── Arithmetic binary ops ───────────────────────────────────────────

        case Add(lhs, rhs)        => binaryArith(BinaryOp.Add, lhs, rhs)
        case Sub(lhs, rhs)        => binaryArith(BinaryOp.Sub, lhs, rhs)
        case Mul(lhs, rhs)        => binaryArith(BinaryOp.Mul, lhs, rhs)
        case Div(lhs, rhs)        => binaryArith(BinaryOp.Div, lhs, rhs)
        case Pow(base, exponent)  => binaryArith(BinaryOp.Pow, base, exponent)

        case Neg(inner) =>
          go(inner).map(a => intern(TensorExpr.Unary(UnaryOp.Neg, a, tpeOf(a))))

        // ── Structural equivalences (transparent wrappers) ──────────────────

        case Fraction(num, den)          => binaryArith(BinaryOp.Div, num, den)
        case Group(inner)                => go(inner)
        case BracketGroup(_, _, inner)   => go(inner)
        case Color(_, inner)             => go(inner)
        case Style(_, inner)             => go(inner)
        case Enclose(_, inner)           => go(inner)

        case Root(None, radicand) =>
          go(radicand).map(a => intern(TensorExpr.Unary(UnaryOp.Sqrt, a, tpeOf(a))))

        case Root(Some(_), _) =>
          Left(LowerError.UnsupportedNode(
            "Root with explicit degree — use Pow(radicand, Fraction(Number(1), degree)) instead"
          ))

        // ── Superscript: only literal-number exponents ──────────────────────

        case Superscript(base, Number(exp)) =>
          go(base).map { b =>
            val c = intern(TensorExpr.Const(exp, TType(DType.F64, Shape.scalar)))
            intern(TensorExpr.Binary(BinaryOp.Pow, b, c, tpeOf(b)))
          }

        case Superscript(_, sup) =>
          Left(LowerError.NonSemanticSuperscript(sup.toString))

        // ── Function calls ──────────────────────────────────────────────────

        case FunctionCall(name, List(arg)) =>
          val opOpt: Option[UnaryOp] = name match
            case "sin"       => Some(UnaryOp.Sin)
            case "cos"       => Some(UnaryOp.Cos)
            case "tan"       => Some(UnaryOp.Tan)
            case "exp"       => Some(UnaryOp.Exp)
            case "log" | "ln" => Some(UnaryOp.Log)
            case "sqrt"      => Some(UnaryOp.Sqrt)
            case "abs"       => Some(UnaryOp.Abs)
            case _           => None
          opOpt match
            case Some(op) => go(arg).map(a => intern(TensorExpr.Unary(op, a, tpeOf(a))))
            case None     => Left(LowerError.UnknownFunction(name, 1))

        case FunctionCall(name, args) =>
          Left(LowerError.UnknownFunction(name, args.length))

        // ── ExprSeq: transparent iff exactly one element ────────────────────

        case ExprSeq(List(inner)) => go(inner)

        case ExprSeq(xs) =>
          Left(LowerError.UnsupportedNode(s"ExprSeq with ${xs.length} elements (expected exactly 1)"))

        // ── Binders — not yet supported ─────────────────────────────────────

        case _: Sum[?]      => Left(LowerError.UnsupportedBinder("Sum"))
        case _: Integral[?] => Left(LowerError.UnsupportedBinder("Integral"))
        case _: Subscript[?] => Left(LowerError.UnsupportedBinder("Subscript"))
        case _: Over[?]     => Left(LowerError.UnsupportedBinder("Over"))
        case _: Under[?]    => Left(LowerError.UnsupportedBinder("Under"))
        case _: SubSup[?]   => Left(LowerError.UnsupportedBinder("SubSup"))

        // ── Leaf nodes with no lowerable content ────────────────────────────

        case Operator(sym)  => Left(LowerError.UnsupportedNode(s"Operator($sym)"))
        case TextNode(text) => Left(LowerError.UnsupportedNode(s"TextNode($text)"))

      end match
    end go

    go(expr).map(out => TensorGraph(nodes.toVector, out))
  end lower

end Lower
