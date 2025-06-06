package check

import breeze.linalg.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import vecxt.all.*
import breeze.numerics.step

@main def breezey =
  // Example data
  val dim = 5
  val dataA = DenseMatrix.rand[Double](dim, dim)
  val dataB = DenseMatrix.rand[Double](dim, dim)
  val vectorData = DenseVector.rand[Double](dim)
  // Random linear algebra workload
  val step1 = dataA + dataB // Element-wise addition

  val step2 = step1 *:* dataA // Hadamard product

  val step3 = step2 * vectorData // Matrix-vector multiply
  println("Step 3 result:")
  println(step3)
  val step4 = step3.map(_ * 2.0 + 1.0) // Element-wise transform
  println("after Step 4 result:")
  println(step3)
  val step5 = breeze.linalg.norm(step4) // L2 norm
  val step6 = step2.t // Transpose
  val step7 = breeze.linalg.sum(step6) // Sum reduction
  val step8 = (step7 > 0.5) // Comparison

  // Combine results to prevent dead code elimination
  val result = step5 + (if step8 then 1.0 else 0.0) + breeze.linalg.max(step4)

  println("Breeze Result:")
  println(result)

  val matA = vecxt.matrix.Matrix(dataA.toArray, (dim, dim))
  val matB = vecxt.matrix.Matrix(dataB.toArray, (dim, dim))
  val vec = vectorData.toArray

  // Same representative linear algebra workload
  val step1Vecxt = matA + matB // Element-wise addition

  val step2Vecxt = step1Vecxt.hadamard(matA) // Hadamard product

  val step3Vecxt = step2Vecxt * vec // Matrix-vector multiply
  val step4Vecxt = step3Vecxt.fma(2.0, 1.0) // Element-wise transform
  val step5Vecxt = vecxt.all.norm(step4Vecxt) // L2 norm
  val step6Vecxt = step2Vecxt.transpose // Transpose
  val step7Vecxt = step6Vecxt.sum // Sum reduction
  val step8Vecxt = (step7Vecxt > 0.5) // Comparison
  // Combine results to prevent dead code elimination
  val resultVecxt = step5Vecxt + (if step8Vecxt then 1.0 else 0.0) + step4Vecxt.maxSIMD

  println("Vecxt Result:")
  println(resultVecxt)

end breezey
