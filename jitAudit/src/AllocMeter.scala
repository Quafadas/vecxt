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
  *   1. Runs a warmup phase (default 20,000 iterations) to let C2 fully compile the path.
  *   2. Measures the hot phase (default 100,000 iterations).
  *   3. The returned value is the raw byte delta; callers divide by reps to get bytes/op.
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

  /** Measures the total heap bytes allocated by {@code body} over {@code reps} iterations, after {@code warmup} warm-up
    * calls. Returns -1 if per-thread allocation tracking is not available.
    */
  def measureAlloc(warmup: Int = 20_000, reps: Int = 100_000)(body: => Unit): Long =
    bean match
      case None    => -1L
      case Some(b) =>
        val tid = Thread.currentThread().threadId()
        // Warmup: let C2 fully compile the path before measuring. With -Xbatch the first
        // call already compiles, but looping ensures profile-directed inlining has stabilised.
        var i = 0
        while i < warmup do
          body
          i += 1
        end while
        val before = b.getThreadAllocatedBytes(tid)
        i = 0
        while i < reps do
          body
          i += 1
        end while
        b.getThreadAllocatedBytes(tid) - before
  end measureAlloc

end AllocMeter
