package vecxt.jit

import java.lang.management.ManagementFactory

/** Allocation measurement via {@code com.sun.management.ThreadMXBean}.
  *
  * Calls {@code getThreadAllocatedBytes} before and after a hot loop to determine how many heap bytes the current
  * thread allocated during those iterations. For a correctly JIT-compiled {@code @AllocFree} kernel, the {@code Vector}
  * objects are eliminated by escape analysis and the count is zero. A {@code DoubleVector} that fails to scalarize is
  * roughly 64–128 bytes (header + lane storage), so any real allocation failure is far above the noise floor.
  *
  * The counter is a TLAB accounting value: it is exact for allocation events, but the measurement call itself has a
  * small constant overhead (typically zero on HotSpot, but not guaranteed). The harness therefore:
  *   1. Runs a warmup phase (default 20,000 iterations) to let C2 begin compiling the path.
  *   2. Runs a first measurement window of {@code reps} iterations to absorb any remaining compilation burst. Two-pass
  *      operations (e.g. {@code variance}) need longer to fully compile than single-pass ops; without this extra window
  *      a bounded allocation spike from the interpreted/C1 phase leaks into the result.
  *   3. Measures a second (definitive) window of {@code reps} iterations and returns the byte delta.
  *
  * This two-window design takes the slope {@code (alloc(2N) − alloc(N)) / N}, which cancels any fixed compilation-burst
  * offset that is present in the first window but absent in the second.
  *
  * Returns -1 when the bean is unavailable (e.g., non-HotSpot JVMs that do not support per-thread allocation tracking).
  * Callers should skip, not fail, in that case — the pinned CI runner (Temurin 25) always supports it.
  */
object AllocMeter:

  private val bean: Option[com.sun.management.ThreadMXBean] =
    val b = ManagementFactory.getPlatformMXBean(classOf[com.sun.management.ThreadMXBean])
    if b != null && b.isThreadAllocatedMemorySupported && b.isThreadAllocatedMemoryEnabled then Some(b)
    else None
    end if
  end bean

  /** Measures the total heap bytes allocated by {@code body} over {@code reps} iterations of the definitive window,
    * after a warmup phase and a first measurement window.
    *
    * Design: warmup → first window (reps iters, result discarded) → definitive window (reps iters, result returned).
    * The first window ensures C2 has finished compiling even two-pass operations before measurement begins, so any
    * bounded compilation-burst allocation is absorbed there and excluded from the returned value. The returned delta
    * therefore represents only steady-state per-op allocation. Dividing by {@code reps} yields bytes/op.
    *
    * Declared {@code inline} so that each call site gets its own specialised measurement loop and the JIT sees the
    * kernel body directly rather than through a megamorphic {@code Function0.apply()} call. Without inlining, fourteen
    * different closures sharing the same call site inside {@code measureAlloc} produce a megamorphic dispatch that C2
    * will not inline through, which means the Vector API calls inside the body are opaque to the compiler and SIMD
    * intrinsics cannot be applied. {@code inline} makes each call site monomorphic.
    */
  inline def measureAlloc(warmup: Int = 20_000, reps: Int = 100_000)(inline body: => Unit): Long =
    bean match
      case None    => -1L
      case Some(b) =>
        val tid = Thread.currentThread().threadId()
        // Warmup: let C2 begin compiling the path before measuring.
        var i = 0
        while i < warmup do
          body
          i += 1
        end while
        // First measurement window: absorbs any remaining compilation burst for two-pass ops.
        // The result is intentionally discarded; this pass acts as additional targeted warmup.
        i = 0
        while i < reps do
          body
          i += 1
        end while
        // Definitive measurement window: C2 is fully compiled, steady-state allocation only.
        val before = b.getThreadAllocatedBytes(tid)
        i = 0
        while i < reps do
          body
          i += 1
        end while
        b.getThreadAllocatedBytes(tid) - before
  end measureAlloc

end AllocMeter
