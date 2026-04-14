package vecxt

object all:
  // Choose not to export this, and import "no" to inline away bounds checking.
  export vecxt.BoundsCheck.DoBoundsCheck.yes

  // arrays
  export vecxt.arrayUtil.*
  export vecxt.doublearrays.*
  export vecxt.floatarrays.*
  export vecxt.intarrays.*

  export vecxt.LongArrays.*
  export vecxt.BooleanArrays.*

  export vecxt.DoubleArraysX.*
  export vecxt.FloatArraysX.*
  export vecxt.IntArraysX.*

  export vecxt.VarianceMode
  export vecxt.ComparisonOp

  // matricies
  export vecxt.OneAndZero.given_OneAndZero_Boolean
  export vecxt.OneAndZero.given_OneAndZero_A
  export vecxt.matrix.*
  export vecxt.matrixUtil.*
  export vecxt.MatrixHelper.*
  export vecxt.MatrixInstance.*
  export vecxt.JvmDoubleMatrix.*
  export vecxt.JvmFloatMatrix.*
  export vecxt.JvmIntMatrix.*
  export vecxt.JsDoubleMatrix.*
  export vecxt.NativeDoubleMatrix.*
  export vecxt.DoubleMatrix.*
  export vecxt.JvmNativeDoubleMatrix.*
  export vecxt.dimensionExtender.DimensionExtender.*

  export vecxt.Determinant.* // Import determinant implementations
  export vecxt.Svd.* // JS and native are stubs
  export vecxt.Cholesky.* // JS and native are stubs
  export vecxt.Eigenvalues.* // JS and native are stubs
  export vecxt.LU.* // JS and native are stubs
  export vecxt.Solve.* // JS and native are stubs
  export vecxt.QR.* // JS and native are stubs
  // Random
  export vecxt.cosineSimilarity
  // ndarray
  export vecxt.ndarray.*
  export vecxt.ndarrayOps.*
  export vecxt.NDArrayDoubleOps.*
  export vecxt.NDArrayReductions.*
  export vecxt.NDArrayIntOps.*
  export vecxt.NDArrayIntReductions.*
  export vecxt.NDArrayBooleanOps.*
  export vecxt.NDArrayFloatOps.*
  export vecxt.NDArrayFloatReductions.*
  export vecxt.NDArrayBooleanIndexing.*
  export vecxt.NDArrayWhere.*
  export vecxt.broadcast.*
end all
