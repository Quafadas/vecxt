package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import vecxt.all.*
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector
import breeze.linalg.*
import vecxt.BoundsCheck.DoBoundsCheck.no

//% mill benchmark_vs_breeze.runJmh -jvmArgs --add-modules=jdk.incubator.vector
@State(Scope.Thread)
class LinearAlgebraWorkloadBenchmark extends BLASBenchmark:

  @Param(Array("500"))
  var matDim: String = uninitialized

  var dataA: Array[Double] = uninitialized
  var dataB: Array[Double] = uninitialized
  var vectorData: Array[Double] = uninitialized

  // Pre-created matrices for pure computation benchmarking
  var breezeMatA: DenseMatrix[Double] = uninitialized
  var breezeMatB: DenseMatrix[Double] = uninitialized
  var breezeVec: DenseVector[Double] = uninitialized

  var vecxtMatA: vecxt.matrix.Matrix[Double] = uninitialized
  var vecxtMatB: vecxt.matrix.Matrix[Double] = uninitialized

  @Setup(Level.Iteration) // Only once per iteration
  def setup(): Unit =
    println(s"[SETUP] Running setup for matrix dim: $matDim")
    val dim = matDim.toInt
    dataA = randomDoubleArray(dim * dim)
    dataB = randomDoubleArray(dim * dim)
    vectorData = randomDoubleArray(dim)

    // Pre-create matrices to exclude construction overhead
    breezeMatA = new DenseMatrix(dim, dim, dataA)
    breezeMatB = new DenseMatrix(dim, dim, dataB)
    breezeVec = new DenseVector(vectorData)

    vecxtMatA = vecxt.matrix.Matrix(dataA, (dim, dim))
    vecxtMatB = vecxt.matrix.Matrix(dataB, (dim, dim))
  end setup

  // @Benchmark
  // def vecxtMinimalTest(bh: Blackhole): Unit =
  //   // Just test one simple operation
  //   val result = vecxtMatA.sum
  //   bh.consume(result)
  // end vecxtMinimalTest

  // @Benchmark
  // def breezeMinimalTest(bh: Blackhole): Unit =
  //   // Just test one simple operation
  //   val result = breeze.linalg.sum(breezeMatA)
  //   bh.consume(result)
  // end breezeMinimalTest

  @Benchmark
  def breezeWorkload(bh: Blackhole): Unit =

    // Representative linear algebra workload
    val step1 = breezeMatA + breezeMatB // Element-wise addition
    val step2 = step1 *:* breezeMatA // Hadamard product
    val step3 = step2 * breezeVec // Matrix-vector multiply
    val step4 = step3.map(_ * 2.0 + 1.0) // Element-wise transform
    val step5 = breeze.linalg.norm(step4) // L2 norm
    // val step6 = step2.t // Transpose
    val step7 = breeze.linalg.sum(step2) // Sum reduction
    val step8 = (step7 > 0.5) // Comparison

    // Combine results to prevent dead code elimination
    val result = step5 + (if step8 then 1.0 else 0.0) + breeze.linalg.max(step4)
    bh.consume(result)
  end breezeWorkload

  @Benchmark
  def vecxtWorkload(bh: Blackhole): Unit =

    // Same representative linear algebra workload
    val step1 = vecxtMatA + vecxtMatB // Element-wise addition
    val step2 = step1.hadamard(vecxtMatA) // Hadamard product
    val step3 = step2 * vectorData // Matrix-vector multiply
    val step4 = step3.fma(2.0, 1.0) // Element-wise transform
    val step5 = step4.norm // L2 norm
    // val step6 = step2.transpose // Transpose
    val step7 = step2.sum // Sum reduction
    val step8 = (step7 > 0.5) // Comparison

    // Combine results to prevent dead code elimination
    val result = step5 + (if step8 then 1.0 else 0.0) + step4.maxSIMD
    bh.consume(result)
  end vecxtWorkload

end LinearAlgebraWorkloadBenchmark
