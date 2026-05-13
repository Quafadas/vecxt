package vecxt.fusion

sealed trait MathExpr[+A]

object MathExpr:
  // Atoms
  case class Number[A](value: A) extends MathExpr[A]
  case class Symbol(name: String) extends MathExpr[Nothing]
  case class Constant(name: String) extends MathExpr[Nothing]

  // Operators
  case class Add[A](lhs: MathExpr[A], rhs: MathExpr[A]) extends MathExpr[A]
  case class Sub[A](lhs: MathExpr[A], rhs: MathExpr[A]) extends MathExpr[A]
  case class Mul[A](lhs: MathExpr[A], rhs: MathExpr[A]) extends MathExpr[A]
  case class Div[A](lhs: MathExpr[A], rhs: MathExpr[A]) extends MathExpr[A]
  case class Pow[A](base: MathExpr[A], exponent: MathExpr[A]) extends MathExpr[A]
  case class Neg[A](expr: MathExpr[A]) extends MathExpr[A]

  // Structures
  case class FunctionCall[A](name: String, args: List[MathExpr[A]]) extends MathExpr[A]
  case class Fraction[A](numerator: MathExpr[A], denominator: MathExpr[A]) extends MathExpr[A]
  case class Root[A](degree: Option[MathExpr[A]], radicand: MathExpr[A]) extends MathExpr[A]
  case class Sum[A](index: MathExpr[A], lower: MathExpr[A], upper: MathExpr[A], body: MathExpr[A]) extends MathExpr[A]
  case class Integral[A](variable: MathExpr[A], lower: MathExpr[A], upper: MathExpr[A], body: MathExpr[A])
      extends MathExpr[A]
  case class Group[A](expr: MathExpr[A]) extends MathExpr[A]

  // Collections
  // Use NDArray

  // Annotations
  case class Subscript[A](base: MathExpr[A], sub: MathExpr[A]) extends MathExpr[A]
  case class Superscript[A](base: MathExpr[A], sup: MathExpr[A]) extends MathExpr[A]

  // AsciiMath-specific nodes
  case class Operator(symbol: String) extends MathExpr[Nothing]
  case class ExprSeq[A](exprs: List[MathExpr[A]]) extends MathExpr[A]
  case class Over[A](base: MathExpr[A], top: MathExpr[A]) extends MathExpr[A]
  case class Under[A](base: MathExpr[A], bottom: MathExpr[A]) extends MathExpr[A]
  case class SubSup[A](base: MathExpr[A], sub: MathExpr[A], sup: MathExpr[A]) extends MathExpr[A]
  case class Style[A](variant: String, content: MathExpr[A]) extends MathExpr[A]
  case class TextNode(content: String) extends MathExpr[Nothing]
  case class BracketGroup[A](open: String, close: String, content: MathExpr[A]) extends MathExpr[A]
  case class Enclose[A](notation: String, content: MathExpr[A]) extends MathExpr[A]
  case class Color[A](color: String, content: MathExpr[A]) extends MathExpr[A]
end MathExpr

/** Convenience alias for the common Double-specialised AST. */
type MathExprD = MathExpr[Double]
