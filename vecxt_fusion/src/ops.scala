package vecxt.fusion

/** Elementwise unary operations. */
enum UnaryOp:
  case Neg, Sin, Cos, Tan, Exp, Log, Sqrt, Abs, Not, Reciprocal
end UnaryOp

/** Elementwise binary operations. */
enum BinaryOp:
  case Add, Sub, Mul, Div, Pow, Min, Max
  case Eq, Neq, Lt, Lte, Gt, Gte
  case And, Or
end BinaryOp

/** Reduction operations along one or more axes. */
enum ReduceOp:
  case Sum, Product, Min, Max, All, Any, ArgMax, ArgMin
end ReduceOp
