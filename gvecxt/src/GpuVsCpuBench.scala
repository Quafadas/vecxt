package vecxt.gpu

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import vecxt.ndarray.NDArray
import io.computenode.cyfra.runtime.VkCyfraRuntime

/** Quick GPU vs CPU (unfused) vs CPU (fused) timing benchmark. */
@main def gpuVsCpuBench(): Unit =
  VkCyfraRuntime.using:
    // warm-up: compile shaders, allocate buffers
    print("Warming up GPU… ")
    val w = GNDArray.fromArray(Array(1.0f, 2.0f, 3.0f))
    val _ = ((w + w) * 2.0f).exp.run
    println("done.")

    // warm-up: fused CPU interpreter
    print("Warming up fused CPU… ")
    val _ = ((w + w) * 2.0f).exp.runCpu
    println("done.")

    // warm-up: fused CPU parallel
    print("Warming up fused CPU parallel… ")
    val _ = ((w + w) * 2.0f).exp.runCpuParallel
    println("done.")

    // warm-up: JVM JIT + BLAS JNI classloading
    print("Warming up unfused CPU… ")
    val cw = NDArray(Array.fill(100)(1.0f), Array(10, 10))
    val _ = ((cw + cw) * 2.0f).exp
    println("done.")

    // warm-up: unfused CPU parallel (coarse-grained chunk split)
    print("Warming up unfused CPU parallel… ")
    val nCores = Runtime.getRuntime.availableProcessors
    val wArr = Array.fill(100)(1.0f)
    cpuUnfusedParallel(wArr, wArr, 10, 10, nCores, lightCpu)
    println(s"done. ($nCores cores)\n")

    // ── Light pipeline: (a + b) * 2 |> exp  (~4 FLOPs/element) ──
    println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    println("  LIGHT pipeline: (a + b) * 2.0 |> exp")
    println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    benchSize(1000, 1000, "light", lightGpu, lightCpu)
    benchSize(3162, 3162, "light", lightGpu, lightCpu)

    // ── Heavy pipeline: chain of transcendentals (~15 FLOPs/element) ──
    println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    println("  HEAVY pipeline: sin(exp(a+b)) * cos(a*b) + atan(a/b) |> exp |> sqrt |> log")
    println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    benchSize(1000, 1000, "heavy", heavyGpu, heavyCpu)
    benchSize(3162, 3162, "heavy", heavyGpu, heavyCpu)
    benchSize(10000, 10000, "heavy", heavyGpu, heavyCpu)

// ── Pipeline definitions (GPU) ─────────────────────────────
private def lightGpu(a: GNDExpr, b: GNDExpr): GNDExpr =
  ((a + b) * 2.0f).exp

private def heavyGpu(a: GNDExpr, b: GNDExpr): GNDExpr =
  val sum = a + b
  val prod = a * b
  val quot = a / b
  val t1 = sum.exp.sin // sin(exp(a+b))
  val t2 = prod.cos // cos(a*b)
  val t3 = quot.atan // atan(a/b)
  ((t1 * t2) + t3).exp.sqrt.log // exp(sin·cos + atan) |> sqrt |> log
end heavyGpu

// ── Pipeline definitions (CPU) ─────────────────────────────
private def lightCpu(a: NDArray[Float], b: NDArray[Float]): NDArray[Float] =
  ((a + b) * 2.0f).exp

private def heavyCpu(a: NDArray[Float], b: NDArray[Float]): NDArray[Float] =
  val sum = a + b
  val prod = a * b
  val quot = a / b
  val t1 = sum.exp.sin // sin(exp(a+b))
  val t2 = prod.cos // cos(a*b)
  val t3 = quot.atan // atan(a/b)
  ((t1 * t2) + t3).exp.sqrt.log
end heavyCpu

/** Coarse-grained parallel unfused CPU: split the flat data across [[nThreads]] chunks and run the SIMD-vectorised
  * [[pipeline]] on each chunk concurrently using a Java parallel IntStream.
  *
  * Each chunk is treated as a 1-D NDArray because all ops are elementwise. The per-chunk results are concatenated back
  * into a single flat array and wrapped in an NDArray with the original shape.
  */
private def cpuUnfusedParallel(
    dataA: Array[Float],
    dataB: Array[Float],
    rows: Int,
    cols: Int,
    nThreads: Int,
    pipeline: (NDArray[Float], NDArray[Float]) => NDArray[Float]
): NDArray[Float] =
  val n = rows * cols
  val chunkSize = math.max(1, (n + nThreads - 1) / nThreads)
  val numChunks = math.min(nThreads, n)
  val chunkResults = new Array[Array[Float]](numChunks)
  java.util.stream.IntStream
    .range(0, numChunks)
    .parallel()
    .forEach: (t: Int) =>
      val start = t * chunkSize
      val end = math.min(start + chunkSize, n)
      val len = end - start
      val chunkA = NDArray(java.util.Arrays.copyOfRange(dataA, start, end), Array(len))
      val chunkB = NDArray(java.util.Arrays.copyOfRange(dataB, start, end), Array(len))
      chunkResults(t) = pipeline(chunkA, chunkB).data
  val out = new Array[Float](n)
  var offset = 0
  var t = 0
  while t < numChunks do
    val chunk = chunkResults(t)
    System.arraycopy(chunk, 0, out, offset, chunk.length)
    offset += chunk.length
    t += 1
  end while
  NDArray(out, Array(rows, cols))
end cpuUnfusedParallel

private def benchSize(
    rows: Int,
    cols: Int,
    label: String,
    gpuPipeline: (GNDExpr, GNDExpr) => GNDExpr,
    cpuPipeline: (NDArray[Float], NDArray[Float]) => NDArray[Float]
)(using io.computenode.cyfra.core.CyfraRuntime): Unit =
  val n = rows * cols
  val nCores = Runtime.getRuntime.availableProcessors
  val rng = new java.util.Random(42)
  val dataA = Array.tabulate(n)(_ => rng.nextFloat().max(0.01f)) // avoid /0 for heavy pipeline
  val dataB = Array.tabulate(n)(_ => rng.nextFloat().max(0.01f))

  println(s"════════════════════════════════════════")
  println(s"  ${rows}×${cols}  ($n elements)  [$label]  [$nCores cores]")
  println(s"════════════════════════════════════════")

  val runs = 3

  // ── GPU runs ─────────────────────────────────────────
  var gpuTotalMs = 0.0
  var gpuResult: Option[GNDLeaf] = None
  logGNDExprTransfers = true
  logGExprTransfers = true
  for run <- 1 to runs do
    val ga = GNDArray.matrix(dataA.clone(), rows, cols)
    val gb = GNDArray.matrix(dataB.clone(), rows, cols)
    val t0 = System.nanoTime()
    val r = gpuPipeline(ga, gb).run
    val t1 = System.nanoTime()
    val ms = (t1 - t0) / 1e6
    println(f"  GPU run $run: $ms%.3f ms")
    gpuTotalMs += ms
    gpuResult = Some(r)
    if run == 1 then
      logGNDExprTransfers = false
      logGExprTransfers = false
    end if
  end for
  val gpuMs = gpuTotalMs / runs

  // ── CPU runs (unfused — separate SIMD loop per op) ────
  var cpuTotalMs = 0.0
  var cpuResult: Option[NDArray[Float]] = None
  for run <- 1 to runs do
    val cpuA = NDArray(dataA.clone(), Array(rows, cols))
    val cpuB = NDArray(dataB.clone(), Array(rows, cols))
    val t2 = System.nanoTime()
    val r = cpuPipeline(cpuA, cpuB)
    val t3 = System.nanoTime()
    val ms = (t3 - t2) / 1e6
    println(f"  CPU unfused run $run: $ms%.3f ms")
    cpuTotalMs += ms
    cpuResult = Some(r)
  end for
  val cpuMs = cpuTotalMs / runs

  // ── CPU runs (unfused parallel — coarse-grained chunk split) ──
  var cpuParTotalMs = 0.0
  var cpuParResult: Option[NDArray[Float]] = None
  for run <- 1 to runs do
    val t2 = System.nanoTime()
    val r = cpuUnfusedParallel(dataA.clone(), dataB.clone(), rows, cols, nCores, cpuPipeline)
    val t3 = System.nanoTime()
    val ms = (t3 - t2) / 1e6
    println(f"  CPU unfused ∥ run $run: $ms%.3f ms")
    cpuParTotalMs += ms
    cpuParResult = Some(r)
  end for
  val cpuParMs = cpuParTotalMs / runs

  // ── CPU fused runs (single pass, no intermediates) ───
  var fusedTotalMs = 0.0
  var fusedResult: Option[GNDLeaf] = None
  for run <- 1 to runs do
    val fa = GNDArray.matrix(dataA.clone(), rows, cols)
    val fb = GNDArray.matrix(dataB.clone(), rows, cols)
    val t4 = System.nanoTime()
    val r = gpuPipeline(fa, fb).runCpu
    val t5 = System.nanoTime()
    val ms = (t5 - t4) / 1e6
    println(f"  CPU fused run $run: $ms%.3f ms")
    fusedTotalMs += ms
    fusedResult = Some(r)
  end for
  val fusedMs = fusedTotalMs / runs

  // ── CPU fused parallel (fine-grained: element loop across cores) ──
  var fusedParTotalMs = 0.0
  var fusedParResult: Option[GNDLeaf] = None
  for run <- 1 to runs do
    val fa = GNDArray.matrix(dataA.clone(), rows, cols)
    val fb = GNDArray.matrix(dataB.clone(), rows, cols)
    val t4 = System.nanoTime()
    val r = gpuPipeline(fa, fb).runCpuParallel
    val t5 = System.nanoTime()
    val ms = (t5 - t4) / 1e6
    println(f"  CPU fused ∥ run $run: $ms%.3f ms")
    fusedParTotalMs += ms
    fusedParResult = Some(r)
  end for
  val fusedParMs = fusedParTotalMs / runs

  // ── Comparison ───────────────────────────────────────
  var maxDiffGpuCpu = 0.0f
  var maxDiffGpuFused = 0.0f
  var maxDiffGpuCpuPar = 0.0f
  var maxDiffGpuFusedPar = 0.0f
  var i = 0
  while i < n do
    val gv = gpuResult.get.data(i)
    val d1 = math.abs(gv - cpuResult.get.data(i))
    val d2 = math.abs(gv - fusedResult.get.data(i))
    val d3 = math.abs(gv - cpuParResult.get.data(i))
    val d4 = math.abs(gv - fusedParResult.get.data(i))
    if d1 > maxDiffGpuCpu then maxDiffGpuCpu = d1
    end if
    if d2 > maxDiffGpuFused then maxDiffGpuFused = d2
    end if
    if d3 > maxDiffGpuCpuPar then maxDiffGpuCpuPar = d3
    end if
    if d4 > maxDiffGpuFusedPar then maxDiffGpuFusedPar = d4
    end if
    i += 1
  end while

  println(f"  GPU avg ($runs):                 $gpuMs%8.3f ms")
  println(f"  CPU unfused avg ($runs):         $cpuMs%8.3f ms")
  println(f"  CPU unfused ∥ avg ($runs):       $cpuParMs%8.3f ms  [$nCores cores, coarse-grained]")
  println(f"  CPU fused avg ($runs):           $fusedMs%8.3f ms")
  println(f"  CPU fused ∥ avg ($runs):         $fusedParMs%8.3f ms  [$nCores cores, fine-grained]")
  println(f"  max |GPU - CPU unfused|:         $maxDiffGpuCpu%.6f")
  println(f"  max |GPU - CPU unfused ∥|:       $maxDiffGpuCpuPar%.6f")
  println(f"  max |GPU - CPU fused|:           $maxDiffGpuFused%.6f")
  println(f"  max |GPU - CPU fused ∥|:         $maxDiffGpuFusedPar%.6f")
  println()
end benchSize
