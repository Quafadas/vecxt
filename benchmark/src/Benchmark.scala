package vecxt.benchmark

import dev.ludovic.netlib.blas.*;

import org.openjdk.jmh.annotations.*;

import java.util.Random;
import java.util.concurrent.TimeUnit;
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.IntVector

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 1)
@Measurement(iterations = 3)
abstract class BLASBenchmark:

  final val spd = DoubleVector.SPECIES_PREFERRED
  final val spi = IntVector.SPECIES_PREFERRED

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

  protected def randomInt(): Double =
    return rand.nextInt();

  protected def randomDoubleArray(n: Int): Array[Double] =
    val res = new Array[Double](n);

    for i <- 0 until n do res(i) = rand.nextDouble();
    end for
    return res;
  end randomDoubleArray

  protected def randomIntArray(n: Int): Array[Int] =
    val res = new Array[Int](n);

    for i <- 0 until n do res(i) = rand.nextInt();
    end for
    return res;
  end randomIntArray

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
