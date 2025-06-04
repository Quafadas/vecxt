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

  @Setup(Level.Invocation)
  def setup(): Unit =
    val dim = matDim.toInt
    dataA = randomDoubleArray(dim * dim)
    dataB = randomDoubleArray(dim * dim)
    vectorData = randomDoubleArray(dim)
  end setup

  @Benchmark
  def breezeWorkload(bh: Blackhole): Unit =
    val dim = matDim.toInt
    // Create matrices and vector from the same data
    val matA = new DenseMatrix(dim, dim, dataA)
    val matB = new DenseMatrix(dim, dim, dataB)
    val vec = new DenseVector(vectorData)

    // Representative linear algebra workload
    val step1 = matA + matB // Element-wise addition
    val step2 = step1 *:* matA // Hadamard product
    val step3 = step2 * vec // Matrix-vector multiply
    val step4 = step3.map(_ * 2.0 + 1.0) // Element-wise transform
    val step5 = breeze.linalg.norm(step4) // L2 norm
    // val step6 = step2.t // Transpose
    val step7 = breeze.linalg.sum(step2) // Sum reduction
    val step8 = (step7 > 0.5) // Comparison

    // Combine results to prevent dead code elimination
    val result = step5 + (if step8 then 1.0 else 0.0)
    bh.consume(result)
  end breezeWorkload

  @Benchmark
  def vecxtWorkload(bh: Blackhole): Unit =
    val dim = matDim.toInt
    // Create matrices and vector from the same data
    val matA = vecxt.matrix.Matrix(dataA, (dim, dim))
    val matB = vecxt.matrix.Matrix(dataB, (dim, dim))

    // Same representative linear algebra workload
    val step1 = matA + matB // Element-wise addition
    val step2 = step1.hadamard(matA) // Hadamard product
    val step3 = step2 * vectorData // Matrix-vector multiply
    val step4 = step3.fma(2.0, 1.0) // Element-wise transform
    val step5 = step4.norm // L2 norm
    // val step6 = step2.transpose // Transpose
    val step7 = step2.sum // Sum reduction
    val step8 = (step7 > 0.5) // Comparison

    // Combine results to prevent dead code elimination
    val result = step5 + (if step8 then 1.0 else 0.0)
    bh.consume(result)
  end vecxtWorkload

end LinearAlgebraWorkloadBenchmark
