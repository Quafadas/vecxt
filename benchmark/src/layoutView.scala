package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import vecxt.all.*

import scala.compiletime.uninitialized

/** Benchmarks view creation itself — `transpose` and `submatrix` — rather than any downstream elementwise work. Every
  * other benchmark in this suite builds its matrices in `@Setup(Level.Trial)`, so a `Layout` allocation never shows up
  * as anything but rounding error. Here it is the entire cost of the `@Benchmark` body, which is the honest place to
  * watch it (vecxt/issues/111, Phase A).
  */
@State(Scope.Thread)
class LayoutViewBenchmark extends BLASBenchmark:

  @Param(Array("10", "100", "1000"))
  var len: String = uninitialized

  var mat: Matrix[Double] = uninitialized

  @Setup(Level.Trial)
  def setup: Unit =
    val n = len.toInt
    mat = Matrix(randomDoubleArray(n * n), (n, n))
    ()
  end setup

  @Benchmark
  def transpose(bh: Blackhole): Matrix[Double] =
    val t = mat.transpose
    bh.consume(t)
    t
  end transpose

  @Benchmark
  def submatrix(bh: Blackhole): Matrix[Double] =
    val n = len.toInt
    val s = mat.submatrix(0 to (n - 1), 0 to (n - 1))
    bh.consume(s)
    s
  end submatrix

end LayoutViewBenchmark
