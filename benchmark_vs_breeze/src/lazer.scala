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
class LazerBenchmark extends BLASBenchmark:

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
    breezeMatA = new DenseMatrix(dim, dim, dataA.clone())
    breezeMatB = new DenseMatrix(dim, dim, dataB.clone())
    breezeVec = new DenseVector(vectorData.clone())

    vecxtMatA = vecxt.matrix.Matrix(dataA.clone(), (dim, dim))
    vecxtMatB = vecxt.matrix.Matrix(dataB.clone(), (dim, dim))
  end setup

  @Benchmark
  def breezeWorkload(bh: Blackhole): Unit =
    val dim = matDim.toInt

    // Representative linear algebra workload
    val step1 = breezeMatA + breezeMatB // Element-wise addition
    val step2 = step1 *:* breezeMatA // Hadamard product
    val step3 = step2 * breezeVec // Matrix-vector multiply
    val step4 = step3.map(_ * 2.0 + 1.0) // Element-wise transform
    val step5 = breeze.linalg.norm(step4) // L2 norm
    val step7 = breeze.linalg.sum(step2) + step5 // Sum reduction
    bh.consume(step7)
  end breezeWorkload

  @Benchmark
  def vecxtWorkload(bh: Blackhole): Unit =
    val dim = matDim.toInt
    // Same representative linear algebra workload
    val step1 = vecxtMatA +:+ vecxtMatB // Element-wise addition
    val step2 = step1.hadamard(vecxtMatA) // Hadamard product
    val step3 = step2 * vectorData // Matrix-vector multiply
    val step4 = step3.fma(2.0, 1.0) // Element-wise transform
    val step5 = step4.norm // L2 norm
    val step7 = step2.sum + step5 // Sum reduction

    bh.consume(step7)
  end vecxtWorkload

end LazerBenchmark
