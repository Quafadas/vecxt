package vecxt

object all:
  // Choose not to export this, and import "no" to inline away bounds checking.
  export vecxt.BoundsCheck.DoBoundsCheck.yes

  // arrays
  export vecxt.arrayUtil.*
  export vecxt.arrays.*
  export vecxt.DoubleArrays.*
  // export vecxt.JsNativeDoubleArrays.*
  export vecxt.BooleanArrays.*

  // matricies
  export vecxt.OneAndZero.given_OneAndZero_Boolean
  export vecxt.OneAndZero.given_OneAndZero_A
  export vecxt.matrix.*
  export vecxt.matrixUtil.*
  export vecxt.MatrixHelper.*
  export vecxt.MatrixInstance.*
  export vecxt.JvmDoubleMatrix.*
  export vecxt.JsDoubleMatrix.*
  export vecxt.NativeDoubleMatrix.*
  export vecxt.DoubleMatrix.*
  export vecxt.JvmNativeDoubleMatrix.*
  export vecxt.dimensionExtender.DimensionExtender.*
  export vecxt.IntArrays.*
  export vecxt.Determinant.* // Import determinant implementations
  export vecxt.Svd.* // JS and native are stubs
  export vecxt.Cholesky.* // JS and native are stubs
  export vecxt.Eigenvalues.* // JS and native are stubs
  export vecxt.LU.* // JS and native are stubs
  export vecxt.Solve.* // JS and native are stubs
  export vecxt.QR.* // JS and native are stubs
  // Random
  export vecxt.cosineSimilarity

  // Longs
  export vecxt.LongArrays.*
end all
