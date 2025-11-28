//> using scala "3.5.2"
//> using javaOpt "--add-modules=jdk.incubator.vector"

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import narr.*

object CheatsheetTest:

  @main def testCheatsheet(): Unit =
    println("=== Testing vecxt Linear Algebra Operations ===\n")

    // Array/Vector Creation and Basic Operations
    println("--- Array/Vector Creation ---")
    val vec = NArray(1.0, 2.0, 3.0)
    println(s"1D array: ${vec.mkString(", ")}")

    println((vec ** 2.0).printArr)

    val mat = Matrix(NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)
    println(s"2D matrix shape: ${mat.shape}")

    println(svd(mat))
    println(rank(mat))

    // QR decomposition
    val matSquare = Matrix(NArray(1.0, 2.0, 3.0, 4.0), 2, 2)
    val (q, r) = qr(matSquare)
    println(s"Q matrix shape: ${q.shape}")
    println(s"R matrix shape: ${r.shape}")

    val zeros = Matrix.zeros[Double]((3, 4))(using summon[scala.reflect.ClassTag[Double]])
    println(s"Zeros matrix shape: ${zeros.shape}")

    val ones = Matrix.ones[Double]((3, 4))(using summon[scala.reflect.ClassTag[Double]])
    println(s"Ones matrix shape: ${ones.shape}")

    val eye = Matrix.eye[Double](3)(using summon[scala.reflect.ClassTag[Double]])
    println(s"Identity matrix shape: ${eye.shape}")

    println(s"Matrix dimensions: rows=${mat.rows}, cols=${mat.cols}")
    println(s"Number of elements: ${mat.numel}")

    // Indexing and Slicing
    println("\n--- Indexing and Slicing ---")
    val m = Matrix(NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0), 3, 3)
    val mBig = Matrix.rand(10, 10)
    m.mean
    println("==== m =====")
    println(m.printMat)
    println("==== m =====")

    println(s"Element at (1, 1): ${m(1, 1)}")
    println(s"Last element in vector: ${vec(vec.length - 1)}")
    println(s"Row 0: ${m.row(0).mkString(", ")}")
    println(s"First 5 rows: ${mBig(0 until 5, ::).printMat}")
    println(s"Last 5 rows: ${mBig(mBig.rows - 5 until mBig.rows, ::).printMat}")
    println(s"submatrix: ${mBig(0 to 2, 1 to 3).printMat}")
    println(s"submatrix: ${m((m.rows - 1 until 0 by -1), ::).printMat}")
    println(s"submatrix: ${m((m.rows - 1 until 0 by -1), ::).layout}")
    println(s"Column 1: ${m.col(1).mkString(", ")}")

    val mt = m.transpose
    println(s"Transposed matrix shape: ${mt.shape}")

    // Element-wise Operations
    println("\n--- Element-wise Operations ---")
    val a = NArray(1.0, 2.0, 3.0, 4.0)
    val b = NArray(2.0, 3.0, 4.0, 5.0)

    val sum = a + b
    println(s"Element-wise addition: ${sum.mkString(", ")}")

    val diff = a - b
    println(s"Element-wise subtraction: ${diff.mkString(", ")}")

    // Element-wise multiply/divide for matrices
    val mA = Matrix(NArray(1.0, 2.0, 3.0, 4.0), 2, 2)
    val mB = Matrix(NArray(2.0, 3.0, 4.0, 5.0), 2, 2)
    val mProd = mA.hadamard(mB)
    println(s"Element-wise multiply (matrix): shape ${mProd.shape}")

    val mQuot = mA /:/ mB
    println(s"Element-wise divide (matrix): shape ${mQuot.shape}")

    val scalarAdd = a + 10.0
    println(s"Scalar addition: ${scalarAdd.mkString(", ")}")

    val scalarMult = a * 2.0
    println(s"Scalar multiplication: ${scalarMult.mkString(", ")}")

    val scalarDiv = a / 2.0
    println(s"Scalar division: ${scalarDiv.mkString(", ")}")

    val negated = -a
    println(s"Negated: ${negated.mkString(", ")}")

    val absolute = a.abs
    println(s"Absolute value: ${absolute.mkString(", ")}")

    // Matrix Operations
    println("\n--- Matrix Operations ---")
    val m1 = Matrix(NArray(1.0, 2.0, 3.0, 4.0), 2, 2)
    val m2 = Matrix(NArray(5.0, 6.0, 7.0, 8.0), 2, 2)

    val mneg = -m1

    val matmul = m1 @@ m2
    println(s"Matrix multiplication result shape: ${matmul.shape}")

    val dotProd = a.dot(b)
    println(s"Dot product: $dotProd")

    val det = m1.det
    println(s"Determinant: $det")

    // Test matrix with scalar operations
    val m3 = Matrix(NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)
    val m3Scaled = m3 * 2.0
    println(s"Matrix scaled by 2: shape ${m3Scaled.shape}")

    val m3Plus = m3 + 5.0
    println(s"Matrix plus 5: shape ${m3Plus.shape}")

    val m3Minus = m3 - 1.0
    println(s"Matrix minus 1: shape ${m3Minus.shape}")

    val m3Div = m3 / 2.0
    println(s"Matrix divided by 2: shape ${m3Div.shape}")

    // Reductions and Aggregations
    println("\n--- Reductions and Aggregations ---")
    println(s"Sum all elements (vector): ${a.sumSIMD}")
    println(s"Sum all elements (matrix): ${m.sum}")

    val sumRows = m.sum(Dimension.Rows)
    val sumRows0 = m.sum(0)
    println(s"Sum along rows: ${sumRows.printMat}")
    println(s"Sum along rows0: ${sumRows0.printMat}")

    val sumCols = m.sum(Dimension.Cols)
    println(s"Sum along columns: ${sumCols.shape}")

    val maxVal = m.raw.max
    println(s"Max value: $maxVal")

    val maxRows = m.max(Dimension.Rows)
    println(s"Max along rows: ${maxRows.shape}")

    val maxCols = m.max(Dimension.Cols)
    println(s"Max along columns: ${maxCols.shape}")

    val minVal = m.raw.min
    println(s"Min value: $minVal")

    val minRows = m.min(Dimension.Rows)
    println(s"Min along rows: ${minRows.shape}")

    val minCols = m.min(Dimension.Cols)
    println(s"Min along columns: ${minCols.shape}")

    val argmaxIdx = a.argmax
    println(s"Argmax index: $argmaxIdx")

    val argminIdx = a.argmin
    println(s"Argmin index: $argminIdx")

    // Norms and Distances
    println("\n--- Norms and Distances ---")
    val v1 = NArray(1.0, 2.0, 3.0)
    val v2 = NArray(4.0, 5.0, 6.0)
    val negv2 = -v2
    val cosSim = cosineSimilarity(v1, v2)
    println(s"Cosine similarity: $cosSim")

    // Mathematical Functions
    println("\n--- Mathematical Functions ---")
    val expResult = a.exp
    println(s"Exponential: ${expResult.mkString(", ")}")

    val logResult = a.log
    println(s"Natural log: ${logResult.mkString(", ")}")

    val log10Result = a.log10
    println(s"Log base 10: ${log10Result.mkString(", ")}")

    val sqrtResult = a.sqrt
    println(s"Square root: ${sqrtResult.mkString(", ")}")

    val sinResult = a.sin
    println(s"Sine: ${sinResult.mkString(", ")}")

    val cosResult = a.cos
    println(s"Cosine: ${cosResult.mkString(", ")}")

    val asinResult = NArray(0.5, 0.7, 0.9).asin
    println(s"Arcsine: ${asinResult.mkString(", ")}")

    val acosResult = NArray(0.5, 0.7, 0.9).acos
    println(s"Arccosine: ${acosResult.mkString(", ")}")

    val atanResult = a.atan
    println(s"Arctangent: ${atanResult.mkString(", ")}")

    val sinhResult = a.sinh
    println(s"Hyperbolic sine: ${sinhResult.mkString(", ")}")

    val coshResult = a.cosh
    println(s"Hyperbolic cosine: ${coshResult.mkString(", ")}")

    // Note: tanh not available on arrays, skipping

    // Matrix mathematical functions
    val mExp = m.exp
    println(s"Matrix exp: shape ${mExp.shape}")

    val mLog = m.log
    println(s"Matrix log: shape ${mLog.shape}")

    val mSqrt = m.sqrt
    println(s"Matrix sqrt: shape ${mSqrt.shape}")

    val mSin = m.sin
    println(s"Matrix sin: shape ${mSin.shape}")

    val mCos = m.cos
    println(s"Matrix cos: shape ${mCos.shape}")

    // Logical Operations
    println("\n--- Logical Operations ---")
    val intArr1 = NArray(1, 2, 3, 4, 5)
    val intArr2 = NArray(3, 2, 3, 1, 6)

    val gtResult = intArr1 > intArr2
    println(s"Greater than: ${gtResult.mkString(", ")}")

    val ltResult = intArr1 < intArr2
    println(s"Less than: ${ltResult.mkString(", ")}")

    val gteResult = intArr1 >= intArr2
    println(s"Greater than or equal: ${gteResult.mkString(", ")}")

    val lteResult = intArr1 <= intArr2
    println(s"Less than or equal: ${lteResult.mkString(", ")}")

    val eqResult = intArr1 =:= intArr2
    println(s"Equality: ${eqResult.mkString(", ")}")

    val neqResult = intArr1 !:= intArr2
    println(s"Inequality: ${neqResult.mkString(", ")}")

    val boolArr = NArray(true, false, true, false, true)
    val boolArr2 = NArray(false, false, true, true, false)

    not(boolArr2)

    // Boolean indexing
    val filtered = a(a > 2.0)
    println(s"Filtered (>2): ${filtered.mkString(", ")}")

    val countTrues = boolArr.trues
    println(s"Count true values: $countTrues")

    // Array Manipulation
    println("\n--- Array Manipulation ---")
    val diagVals = m.diag
    println(s"Diagonal: ${diagVals.mkString(", ")}")

    val uniqueVals = NArray(1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0).unique
    println(s"Unique values: ${uniqueVals.mkString(", ")}")

    val toSort = NArray(3.0, 1.0, 4.0, 1.0, 5.0)
    val sorted = narr.copy(toSort)
    narr.sort(sorted)()
    println(s"Sorted: ${sorted.mkString(", ")}")

    // Special Operations
    println("\n--- Special Operations ---")
    val copied = narr.copy(a)
    println(s"Copied array: ${copied.mkString(", ")}")

    val increments = intArr1.increments
    println(s"Increments (diff): ${increments.mkString(", ")}")

    val logSumExpVal = a.logSumExp
    println(s"Log-sum-exp: $logSumExpVal")

    val filled = Matrix.fill(7.0, (2, 3))
    println(s"Filled matrix shape: ${filled.shape}")

    println("\n=== All implemented operations tested successfully! ===")
  end testCheatsheet

end CheatsheetTest
