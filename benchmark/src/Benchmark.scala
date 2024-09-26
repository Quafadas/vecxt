package vecxt.benchmark

import dev.ludovic.netlib.blas.*;

import org.openjdk.jmh.annotations.*;

import java.util.Random;
import java.util.concurrent.TimeUnit;

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 3)
@Measurement(iterations = 3)
abstract class BLASBenchmark:

  var blas: BLAS = _;

  @Setup
  def setupImplementation: Unit =
    blas = JavaBLAS.getInstance();
    ()

    // System.out.println("implementation = " + blas.getClass().getName());
  end setupImplementation

  private final val rand: Random = new Random(0);

  protected def randomDouble(): Double =
    return rand.nextDouble();

  protected def randomDoubleArray(n: Int): Array[Double] =
    val res = new Array[Double](n);

    for i <- 0 until n do res(i) = rand.nextDouble();
    end for
    return res;
  end randomDoubleArray

  protected def randomBooleanArray(n: Int): Array[Boolean] =
    val res = new Array[Boolean](n);

    for i <- 0 until n do res(i) = rand.nextDouble() < 0.5;
    end for
    return res;
  end randomBooleanArray

  protected def randomFloat(): Float =
    return rand.nextFloat();

  protected def randomFloatArray(n: Int): Array[Float] =
    val res = new Array[Float](n);
    for i <- 0 until n do res(i) = rand.nextFloat();
    end for
    return res;
  end randomFloatArray
end BLASBenchmark
