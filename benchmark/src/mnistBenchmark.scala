package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import vecxt.all.*
import vecxt.BoundsCheck
import BoundsCheck.DoBoundsCheck.no
import scala.compiletime.uninitialized

import java.util.concurrent.TimeUnit

/** Breaks down the MNIST forward + backward pass into individual operations
  * to identify where time is spent. Uses realistic dimensions from the MNIST
  * training loop: batchSize x 784 input, 784x128 hidden, 128x10 output.
  */

// ./mill benchmark.runJmh "vecxt.benchmark.MnistBenchmark" -jvmArgs "--add-modules=jdk.incubator.vector"
@State(Scope.Thread)
class MnistBenchmark extends BLASBenchmark:

  // MNIST network dimensions
  val imageSize = 784  // 28*28
  val hiddenSize = 128
  val outputSize = 10

  @Param(Array("128", "512"))
  var batchSize: String = uninitialized

  var bs: Int = uninitialized

  // Forward pass inputs
  var xBatch: Matrix[Float] = uninitialized   // (bs, 784)
  var w1: Matrix[Float] = uninitialized        // (784, 128)
  var b1: Array[Float] = uninitialized         // (128)
  var w2: Matrix[Float] = uninitialized        // (128, 10)
  var b2: Array[Float] = uninitialized         // (10)

  // Forward pass intermediates (for backward benchmarks)
  var z1: Matrix[Float] = uninitialized        // (bs, 128)
  var a1: Matrix[Float] = uninitialized        // (bs, 128)
  var z2: Matrix[Float] = uninitialized        // (bs, 10)
  var a2: Matrix[Float] = uninitialized        // (bs, 10)
  var yBatch: Matrix[Float] = uninitialized    // (bs, 10) one-hot

  // Backward intermediates
  var dz2: Matrix[Float] = uninitialized       // (bs, 10)
  var dz1: Matrix[Float] = uninitialized       // (bs, 128)
  var dz1Check: Matrix[Boolean] = uninitialized
  var a1T: Matrix[Float] = uninitialized       // (128, bs)
  var xT: Matrix[Float] = uninitialized        // (784, bs)
  var w2T: Matrix[Float] = uninitialized       // (10, 128)

  // Weight update intermediates
  var dw1: Matrix[Float] = uninitialized
  var dw2: Matrix[Float] = uninitialized
  var db1: Array[Float] = uninitialized
  var db2: Array[Float] = uninitialized

  @Setup(Level.Trial)
  def setup: Unit =
    bs = batchSize.toInt

    xBatch = Matrix(randomFloatArray(bs * imageSize), (bs, imageSize))
    w1 = Matrix(randomFloatArray(imageSize * hiddenSize).map(_ * 0.2f), (imageSize, hiddenSize))
    b1 = randomFloatArray(hiddenSize)
    w2 = Matrix(randomFloatArray(hiddenSize * outputSize).map(_ * 0.2f), (hiddenSize, outputSize))
    b2 = randomFloatArray(outputSize)

    // One-hot labels
    val yRaw = Array.fill(bs * outputSize)(0.0f)
    val rng = new java.util.Random(42)
    var i = 0
    while i < bs do
      yRaw(i + bs * rng.nextInt(outputSize)) = 1.0f
      i += 1
    end while
    yBatch = Matrix(yRaw, (bs, outputSize))

    // Pre-compute forward pass intermediates for backward benchmarks
    z1 = xBatch @@ w1
    z1.mapRowsInPlace(r => { r += b1; r })
    a1 = Matrix(z1.raw.clampMin(0.0f), z1.shape)
    z2 = a1 @@ w2
    z2.mapRowsInPlace(r => { r += b2; r })
    a2 = softmaxRowsBench(z2.deepCopy)

    dz2 = a2 - yBatch
    dz1Check = z1 > 0
    a1T = a1.transpose
    xT = xBatch.transpose
    w2T = w2.transpose

    dz1 = (dz2 @@ w2T)
    dz1 *:*= dz1Check

    val m_inv = 1.0f / bs
    dw1 = m_inv * (xT @@ dz1)
    dw2 = m_inv * (a1T @@ dz2)
    db1 = dz1.mapColsToScalar(r => r.sumSIMD * m_inv).raw
    db2 = dz2.mapColsToScalar(_.sum).raw
    ()
  end setup

  // ============================================================
  // FORWARD PASS — individual operations
  // ============================================================

  @Benchmark
  def fwd_01_matmul_x_w1(bh: Blackhole): Unit =
    // (bs, 784) @@ (784, 128) — the big matmul
    bh.consume(xBatch @@ w1)

  @Benchmark
  def fwd_02_bias_add_b1(bh: Blackhole): Unit =
    val z = z1.deepCopy
    z.mapRowsInPlace(r => { r += b1; r })
    bh.consume(z)

  @Benchmark
  def fwd_03_relu(bh: Blackhole): Unit =
    bh.consume(Matrix(z1.raw.clampMin(0.0f), z1.shape))

  @Benchmark
  def fwd_04_matmul_a1_w2(bh: Blackhole): Unit =
    // (bs, 128) @@ (128, 10)
    bh.consume(a1 @@ w2)

  @Benchmark
  def fwd_05_bias_add_b2(bh: Blackhole): Unit =
    val z = z2.deepCopy
    z.mapRowsInPlace(r => { r += b2; r })
    bh.consume(z)

  @Benchmark
  def fwd_06_softmax(bh: Blackhole): Unit =
    bh.consume(softmaxRowsBench(z2.deepCopy))

  @Benchmark
  def fwd_full_forward(bh: Blackhole): Unit =
    val z1_ = xBatch @@ w1
    z1_.mapRowsInPlace(r => { r += b1; r })
    val a1_ = Matrix(z1_.raw.clampMin(0.0f), z1_.shape)
    val z2_ = a1_ @@ w2
    z2_.mapRowsInPlace(r => { r += b2; r })
    val a2_ = softmaxRowsBench(z2_)
    bh.consume(a2_)

  // ============================================================
  // BACKWARD PASS — individual operations
  // ============================================================

  @Benchmark
  def bwd_01_dz2_sub(bh: Blackhole): Unit =
    bh.consume(a2 - yBatch)

  @Benchmark
  def bwd_02_matmul_a1T_dz2(bh: Blackhole): Unit =
    // (128, bs) @@ (bs, 10) — gradient for w2
    bh.consume(a1T @@ dz2)

  @Benchmark
  def bwd_03_transpose_a1(bh: Blackhole): Unit =
    bh.consume(a1.transpose)

  @Benchmark
  def bwd_04_db2_col_sum(bh: Blackhole): Unit =
    bh.consume(dz2.mapColsToScalar(_.sum).raw)

  @Benchmark
  def bwd_05_relu_mask(bh: Blackhole): Matrix[Boolean] =
    val result = z1 > 0
    bh.consume(result)
    result

  @Benchmark
  def bwd_06_matmul_dz2_w2T(bh: Blackhole): Unit =
    // (bs, 10) @@ (10, 128) — propagate error back
    bh.consume(dz2 @@ w2T)

  @Benchmark
  def bwd_07_mask_multiply(bh: Blackhole): Unit =
    val dz = (dz2 @@ w2T)
    dz *:*= dz1Check
    bh.consume(dz)

  @Benchmark
  def bwd_07b_zeroWhere(bh: Blackhole): Unit =
    // Fused alternative: single SIMD pass, no boolean allocation
    val dz = (dz2 @@ w2T)
    dz.raw.`zeroWhere!`(z1.raw, 0.0f, ComparisonOp.LE)
    bh.consume(dz)

  @Benchmark
  def bwd_08_matmul_xT_dz1(bh: Blackhole): Unit =
    // (784, bs) @@ (bs, 128) — gradient for w1, the big one
    bh.consume(xT @@ dz1)

  @Benchmark
  def bwd_09_db1_col_sum(bh: Blackhole): Unit =
    val m_inv = 1.0f / bs
    bh.consume(dz1.mapColsToScalar(r => r.sumSIMD * m_inv).raw)

  @Benchmark
  def bwd_full_backward(bh: Blackhole): Unit =
    val m_inv = 1.0f / bs
    val dz2_ = a2 - yBatch
    val dw2_ = m_inv * (a1T @@ dz2_)
    val db2_ = dz2_.mapColsToScalar(_.sum).raw
    val dz1_ = (dz2_ @@ w2T)
    dz1_.raw.`zeroWhere!`(z1.raw, 0.0f, ComparisonOp.LE)
    val dw1_ = m_inv * (xT @@ dz1_)
    val db1_ = dz1_.mapColsToScalar(r => r.sumSIMD * m_inv).raw
    bh.consume(dw1_)

  // ============================================================
  // WEIGHT UPDATE
  // ============================================================

  // @Benchmark
  // def upd_01_w1_update(bh: Blackhole): Unit =
  //   import BoundsCheck.DoBoundsCheck.yes
  //   val w = w1.deepCopy
  //   w -= (dw1 * 0.05f)
  //   bh.consume(w)

  // @Benchmark
  // def upd_02_w2_update(bh: Blackhole): Unit =
  //   import BoundsCheck.DoBoundsCheck.yes
  //   val w = w2.deepCopy
  //   w -= (dw2 * 0.05f)
  //   bh.consume(w)

  // @Benchmark
  // def upd_03_b1_update(bh: Blackhole): Unit =
  //   val b = b1.clone()
  //   b -= (db1 * 0.05f)
  //   bh.consume(b)

  // ============================================================
  // FULL STEP (forward + backward + update) for reference
  // ============================================================

  @Benchmark
  def full_training_step(bh: Blackhole): Unit =
    val alpha = 0.05f
    val m_inv = 1.0f / bs
    // Forward
    val z1_ = xBatch @@ w1
    z1_.mapRowsInPlace(r => { r += b1; r })
    val a1_ = Matrix(z1_.raw.clampMin(0.0f), z1_.shape)
    val z2_ = a1_ @@ w2
    z2_.mapRowsInPlace(r => { r += b2; r })
    val a2_ = softmaxRowsBench(z2_)
    // Backward
    val dz2_ = a2_ - yBatch
    val dw2_ = m_inv * (a1_.transpose @@ dz2_)
    val db2_ = dz2_.mapColsToScalar(_.sum).raw
    val dz1_ = dz2_ @@ w2.transpose
    dz1_.raw.`zeroWhere!`(z1_.raw, 0.0f, ComparisonOp.LE)
    val dw1_ = m_inv * (xBatch.transpose @@ dz1_)
    val db1_ = dz1_.mapColsToScalar(r => r.sumSIMD * m_inv).raw
    // Update (consume results)
    bh.consume(dw1_)
    bh.consume(dw2_)
    bh.consume(db1_)
    bh.consume(db2_)

  private def softmaxRowsBench(z: Matrix[Float]): Matrix[Float] =
    z.mapRows { row =>
      row -= row.max
      row.`exp!`
      row /= row.sum
      row
    }

end MnistBenchmark
