//> using scala "3.5.2"
//> using javaOpt "--add-modules=jdk.incubator.vector"

import vecxt.all.*

object CheatsheetTest:

  @main def testCheatsheet(): Unit =
    println("=== Testing vecxt Linear Algebra Operations ===\n")

    // Array/Vector Creation and Basic Operations
    println("--- Array/Vector Creation ---")
    val vec = Array(1.0, 2.0, 3.0)
    println(s"1D array: ${vec.mkString(", ")}")

    println((vec ** 2.0).printArr)

    val mat = Matrix(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)
    println(s"2D matrix shape: ${mat.shape}")

    println(svd(mat))
    println(rank(mat))

    // QR decomposition
    val matSquare = Matrix(Array(1.0, 2.0, 3.0, 4.0), 2, 2)
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
    val m = Matrix(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0), 3, 3)
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
    val a = Array(1.0, 2.0, 3.0, 4.0)
    val b = Array(2.0, 3.0, 4.0, 5.0)

    val sum = a + b
    println(s"Element-wise addition: ${sum.mkString(", ")}")

    val diff = a - b
    println(s"Element-wise subtraction: ${diff.mkString(", ")}")

    // Element-wise multiply/divide for matrices
    val mA = Matrix(Array(1.0, 2.0, 3.0, 4.0), 2, 2)
    val mB = Matrix(Array(2.0, 3.0, 4.0, 5.0), 2, 2)
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
    val m1 = Matrix(Array(1.0, 2.0, 3.0, 4.0), 2, 2)
    val m2 = Matrix(Array(5.0, 6.0, 7.0, 8.0), 2, 2)

    val mneg = -m1

    val matmul = m1 @@ m2
    println(s"Matrix multiplication result shape: ${matmul.shape}")

    val dotProd = a.dot(b)
    println(s"Dot product: $dotProd")

    val det = m1.det
    println(s"Determinant: $det")

    // Test matrix with scalar operations
    val m3 = Matrix(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)
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
    val v1 = Array(1.0, 2.0, 3.0)
    val v2 = Array(4.0, 5.0, 6.0)
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

    val asinResult = Array(0.5, 0.7, 0.9).asin
    println(s"Arcsine: ${asinResult.mkString(", ")}")

    val acosResult = Array(0.5, 0.7, 0.9).acos
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
    val intArr1 = Array(1, 2, 3, 4, 5)
    val intArr2 = Array(3, 2, 3, 1, 6)

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

    val boolArr = Array(true, false, true, false, true)
    val boolArr2 = Array(false, false, true, true, false)

    not(boolArr2)

    // Boolean indexing
    val filtered = a.mask(a > 2.0)
    println(s"Filtered (>2): ${filtered.mkString(", ")}")

    val countTrues = boolArr.trues
    println(s"Count true values: $countTrues")

    // Array Manipulation
    println("\n--- Array Manipulation ---")
    val diagVals = m.diag
    println(s"Diagonal: ${diagVals.mkString(", ")}")

    val uniqueVals = Array(1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0).unique
    println(s"Unique values: ${uniqueVals.mkString(", ")}")

    val toSort = Array(3.0, 1.0, 4.0, 1.0, 5.0)
    val sorted = toSort.clone()
    scala.util.Sorting.quickSort(sorted)
    println(s"Sorted: ${sorted.mkString(", ")}")

    // Special Operations
    println("\n--- Special Operations ---")
    val copied = a.clone()
    println(s"Copied array: ${copied.mkString(", ")}")

    val increments = intArr1.increments
    println(s"Increments (diff): ${increments.mkString(", ")}")

    val logSumExpVal = a.logSumExp
    println(s"Log-sum-exp: $logSumExpVal")

    val filled = Matrix.fill(7.0, (2, 3))
    println(s"Filled matrix shape: ${filled.shape}")

    // ─────────────────────────────────────────────────────────────────────────
    // Non-Double element types.
    //
    // Everything above this line is Double. That mattered for vecxt/issues/105:
    // the C6a bytecode audit can only find a specialization failure on a code
    // path something actually calls, so element types with no call site here
    // were simply unaudited. These exercise Float, Int, Long and Boolean across
    // Array, NDArray and Matrix so the audit has real usage to look at.
    // ─────────────────────────────────────────────────────────────────────────

    println("\n--- Float arrays ---")
    val fa = Array(1.0f, 2.0f, 3.0f, 4.0f)
    val fb = Array(2.0f, 3.0f, 4.0f, 5.0f)
    println(s"add: ${(fa + fb).mkString(", ")}")
    println(s"subtract: ${(fa - fb).mkString(", ")}")
    println(s"scalar multiply: ${(fa * 2.0f).mkString(", ")}")
    println(s"scalar divide: ${(fa / 2.0f).mkString(", ")}")
    println(s"power: ${(fa ** 2.0f).mkString(", ")}")
    println(s"sqrt: ${fa.sqrt.mkString(", ")}")
    println(s"exp: ${fa.exp.mkString(", ")}")
    println(s"cumsum: ${fa.cumsum.mkString(", ")}")
    println(s"sum / sumSIMD: ${fa.sum} / ${fa.sumSIMD}")
    println(s"mean: ${fa.mean}")
    println(s"min / max: ${fa.min} / ${fa.max}")
    println(s"argmax: ${fa.argmax}")
    println(s"dot: ${fa.dot(fb)}")
    println(s"norm: ${fa.norm}")
    println(s"variance / std: ${fa.variance(VarianceMode.Sample)} / ${fa.std(VarianceMode.Sample)}")

    println("\n--- Int arrays ---")
    val ia = Array(1, 2, 3, 4, 5)
    val ib = Array(5, 4, 3, 2, 1)
    println(s"add: ${(ia + ib).mkString(", ")}")
    println(s"subtract: ${(ia - ib).mkString(", ")}")
    println(s"scalar subtract: ${(ia - 1).mkString(", ")}")
    println(s"divide to Double: ${(ia / 2.0).mkString(", ")}")
    println(s"multiply by Float: ${(ia * 2.0f).mkString(", ")}")
    println(s"sumSIMD: ${ia.sumSIMD}")
    println(s"mean: ${ia.mean}")
    println(s"minSIMD / maxSIMD: ${ia.minSIMD} / ${ia.maxSIMD}")
    println(s"dot: ${ia.dot(ib)}")
    println(s"std: ${ia.std}")
    println(s"increments: ${ia.increments.mkString(", ")}")

    println("\n--- Long arrays ---")
    val la = Array(1L, 2L, 3L, 4L)
    println(s"sumSIMD: ${la.sumSIMD}")
    println(s"select: ${la.select(Array(0, 2)).mkString(", ")}")

    println("\n--- Boolean arrays ---")
    val ba1 = Array(true, false, true, false)
    val ba2 = Array(true, true, false, false)
    println(s"and: ${(ba1 && ba2).mkString(", ")}")
    println(s"or: ${(ba1 || ba2).mkString(", ")}")
    println(s"not: ${ba1.not.mkString(", ")}")
    println(s"any: ${ba1.any}")
    println(s"allTrue: ${ba1.allTrue}")
    println(s"trues: ${ba1.trues}")

    println("\n--- NDArray[Float] ---")
    val ndf = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val ndf2 = NDArray(Array(6.0f, 5.0f, 4.0f, 3.0f, 2.0f, 1.0f), Array(2, 3))
    println(s"add: ${(ndf + ndf2).toArray.mkString(", ")}")
    println(s"multiply: ${(ndf * ndf2).toArray.mkString(", ")}")
    println(s"scalar add: ${(ndf + 1.0f).toArray.mkString(", ")}")
    println(s"sqrt: ${ndf.sqrt.toArray.mkString(", ")}")
    println(s"sum / mean: ${ndf.sum} / ${ndf.mean}")
    println(s"min / max: ${ndf.min} / ${ndf.max}")
    println(s"product / variance: ${ndf.product} / ${ndf.variance}")
    println(s"norm / argmax: ${ndf.norm} / ${ndf.argmax}")
    println(s"sum along axis 0: ${ndf.sum(0).toArray.mkString(", ")}")
    // Non-monotonic index array forces the copy/gather path rather than a view.
    println(s"gather rows: ${ndf(Array(1, 0), ::).toArray.mkString(", ")}")

    println("\n--- NDArray[Int] ---")
    val ndi = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val ndi2 = NDArray(Array(6, 5, 4, 3, 2, 1), Array(2, 3))
    println(s"add: ${(ndi + ndi2).toArray.mkString(", ")}")
    println(s"multiply: ${(ndi * ndi2).toArray.mkString(", ")}")
    println(s"modulo: ${(ndi % 3).toArray.mkString(", ")}")
    println(s"sum / mean: ${ndi.sum} / ${ndi.mean}")
    println(s"min / max: ${ndi.min} / ${ndi.max}")
    println(s"product / argmax: ${ndi.product} / ${ndi.argmax}")
    println(s"max along axis 0: ${ndi.max(0).toArray.mkString(", ")}")
    println(s"gather rows: ${ndi(Array(1, 0), ::).toArray.mkString(", ")}")

    println("\n--- NDArray[Boolean] ---")
    val ndb1 = NDArray(Array(true, false, true, false, true, false), Array(2, 3))
    val ndb2 = NDArray(Array(true, true, false, false, true, true), Array(2, 3))
    println(s"and: ${(ndb1 && ndb2).toArray.mkString(", ")}")
    println(s"or: ${(ndb1 || ndb2).toArray.mkString(", ")}")
    println(s"not: ${ndb1.not.toArray.mkString(", ")}")
    println(s"any / all: ${ndb1.any} / ${ndb1.all}")
    println(s"countTrue: ${ndb1.countTrue}")
    println(s"countTrue along axis 0: ${ndb1.countTrue(0).toArray.mkString(", ")}")
    // Boolean mask indexing back into a numeric NDArray.
    println(s"masked select: ${ndf(ndb1).toArray.mkString(", ")}")

    println("\n--- Matrix[Float] ---")
    val fm1 = Matrix(Array(1.0f, 2.0f, 3.0f, 4.0f), 2, 2)
    val fm2 = Matrix(Array(5.0f, 6.0f, 7.0f, 8.0f), 2, 2)
    println(s"matmul shape: ${(fm1 @@ fm2).shape}")
    println(s"matmul with alpha/beta shape: ${fm1.matmul(fm2, 1.0f, 0.0f).shape}")
    println(s"scaled: ${(fm1 * 2.0f).printMat}")
    println(s"plus scalar: ${(fm1 + 1.0f).printMat}")
    println(s"colSums: ${fm1.colSums.mkString(", ")}")
    println(s"sum along rows shape: ${fm1.sum(Dimension.Rows).shape}")
    println(s"max along cols shape: ${fm1.max(Dimension.Cols).shape}")
    println(s"ge mask: ${(fm1 >= 2.0f).printMat}")
    println(s"transpose: ${fm1.transpose.printMat}")
    println(s"row 0: ${fm1.row(0).mkString(", ")}")

    println("\n--- Matrix[Int] ---")
    val im1 = Matrix(Array(1, 2, 3, 4), 2, 2)
    val im2 = Matrix(Array(5, 6, 7, 8), 2, 2)
    println(s"matmul shape: ${im1.matmul(im2).shape}")
    println(s"divide to Double shape: ${(im1 / 2.0).shape}")
    println(s"divide to Float shape: ${(im1 / 2.0f).shape}")
    println(s"sum along rows shape: ${im1.sum(Dimension.Rows).shape}")
    println(s"min along cols shape: ${im1.min(Dimension.Cols).shape}")
    println(s"gt mask: ${(im1 >= 2).printMat}")
    println(s"transpose: ${im1.transpose.printMat}")
    println(s"diag: ${im1.diag.mkString(", ")}")

    println("\n--- Matrix[Boolean] ---")
    val bm = Matrix(Array(true, false, true, false), 2, 2)
    println(s"printMat: ${bm.printMat}")
    println(s"transpose: ${bm.transpose.printMat}")
    println(s"masked float matrix: ${(fm1 *:* bm).printMat}")

    println("\n=== All implemented operations tested successfully! ===")
  end testCheatsheet

end CheatsheetTest
