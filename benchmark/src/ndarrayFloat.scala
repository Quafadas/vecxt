package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import vecxt.all.*
import scala.compiletime.uninitialized

// format: off
/**
 * Baseline (flatUnaryOp scalar loop) vs SIMD (FloatArraysCrossCompat) for NDArray[Float] unary ops.
 *
 * Run with:
 *   ./mill benchmark.runMain vecxt.benchmark.NDArrayFloatBenchmark
 * or via JMH:
 *   ./mill benchmark.runJmh -t 1 -f 1 -wi 1 -i 3 "NDArrayFloat.*"
 */
// format: on
@State(Scope.Thread)
class NDArrayFloatBenchmark extends BLASBenchmark:

  @Param(Array("10", "1000", "100000"))
  var n: java.lang.String = uninitialized

  var arr: NDArray[Float] = uninitialized
  var arr2: NDArray[Float] = uninitialized

  @Setup(Level.Trial)
  def setup: Unit =
    val data = Array.tabulate(n.toInt)(i => (i + 1).toFloat)
    arr = NDArray(data, Array(n.toInt))
    arr2 = NDArray(data.map(_ * 2.0f), Array(n.toInt))
    ()
  end setup

  @Benchmark
  def ndarray_float_add(bh: Blackhole): Unit =
    bh.consume(arr + arr2)

  @Benchmark
  def ndarray_float_mul_scalar(bh: Blackhole): Unit =
    bh.consume(arr * 2.0f)

  @Benchmark
  def ndarray_float_exp(bh: Blackhole): Unit =
    bh.consume(arr.exp)

  @Benchmark
  def ndarray_float_log(bh: Blackhole): Unit =
    bh.consume(arr.log)

  @Benchmark
  def ndarray_float_sqrt(bh: Blackhole): Unit =
    bh.consume(arr.sqrt)

  @Benchmark
  def ndarray_float_tanh(bh: Blackhole): Unit =
    bh.consume(arr.tanh)

  @Benchmark
  def ndarray_float_abs(bh: Blackhole): Unit =
    bh.consume(arr.abs)

end NDArrayFloatBenchmark
