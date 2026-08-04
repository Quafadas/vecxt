package vecxt.jit

import java.lang.management.ManagementFactory

/** Allocation measurement via {@code com.sun.management.ThreadMXBean}.
  *
  * Uses a slope approach: captures allocation over two consecutive windows of length {@code reps} and {@code 2*reps}
  * within a single hot loop, then returns {@code max(0, alloc_2N − alloc_N)}. Dividing the returned value by
  * {@code reps} yields the steady-state bytes/op.
  *
  * <h3>Why the slope?</h3>
  *
  * A bounded JIT-compilation burst — unscalarised {@code DoubleVector} objects from the interpreted/C1 phase of a
  * callee that reaches its own compile threshold during the measurement window — contributes a fixed lump sum to the
  * first measurement window ({@code alloc_N}) but does not recur in the second ({@code alloc_2N}). Taking the
  * difference cancels it:
  *
  * <pre> alloc_N = burst + N × per_op alloc_2N = 2N × per_op (burst is over, steady state) slope = (alloc_2N − alloc_N)
  * / N = per_op − burst/N </pre>
  *
  * When {@code per_op = 0} (correctly scalarised), {@code slope ≤ 0}; the {@code max(0, …)} clamp returns zero. When a
  * {@code DoubleVector} fails to scalarise ({@code per_op ≈ 64+ bytes}), {@code slope = per_op − burst/N}. With the
  * observed CI burst of ≈1.5 MB over {@code N = 100 000} iterations ({@code burst/N ≈ 15 bytes/op}), a scalarisation
  * failure still produces {@code slope ≈ 64 − 15 = 49 bytes/op}, well above the 8-byte epsilon.
  *
  * <h3>Why a single loop?</h3>
  *
  * Both windows sit inside the same already-compiled loop, so there is no inter-window safepoint that could deoptimise
  * the test-method frame and restart the JIT-compile burst in the second window.
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

  /** Measures steady-state heap allocation using a slope approach. Returns {@code max(0, alloc_2N − alloc_N)} where
    * {@code alloc_N} is bytes allocated in the first {@code reps}-iteration window and {@code alloc_2N} is bytes
    * allocated in the following {@code 2*reps}-iteration window. Returns -1 if per-thread allocation tracking is not
    * available.
    *
    * A bounded JIT-compilation burst lands in {@code alloc_N} but not {@code alloc_2N}; the difference cancels it so a
    * truly alloc-free kernel returns zero. Steady-state per-op allocation (e.g. from a real scalarisation failure)
    * appears in both windows proportionally and survives the subtraction. Callers divide the returned value by
    * {@code reps} to obtain bytes/op.
    *
    * Declared {@code inline} so that each call site gets its own specialised measurement loop and the JIT sees the
    * kernel body directly rather than through a megamorphic {@code Function0.apply()} call. Without inlining, many
    * different closures sharing the same call site inside {@code measureAlloc} produce a megamorphic dispatch that C2
    * will not inline through, preventing SIMD intrinsification.
    */
  inline def measureAlloc(warmup: Int = 20_000, reps: Int = 100_000)(inline body: => Unit): Long =
    bean match
      case None    => -1L
      case Some(b) =>
        val tid = Thread.currentThread().threadId()
        // Single loop: warmup iters (OSR compiles here), then window1 (reps iters), then window2 (2*reps iters).
        // Three getThreadAllocatedBytes checkpoints, all fired as cold one-shot branches inside the compiled loop:
        //   before1 at window1 start, before2 at window2 start, after2 outside the loop.
        // alloc_N  = before2 - before1  (reps iterations; may include a JIT-compilation burst)
        // alloc_2N = after2  - before2  (2*reps iterations; steady-state, burst is over)
        // returned = max(0, alloc_2N - alloc_N)  →  slope * reps, burst-cancelled
        val window1Start = warmup
        val window2Start = warmup + reps
        val total = warmup + reps + 2 * reps // warmup + 3 * reps
        var before1 = 0L
        var before2 = 0L
        var i = 0
        while i < total do
          if i == window1Start then before1 = b.getThreadAllocatedBytes(tid)
          end if
          if i == window2Start then before2 = b.getThreadAllocatedBytes(tid)
          end if
          body
          i += 1
        end while
        val after2 = b.getThreadAllocatedBytes(tid)
        val alloc1 = before2 - before1 // reps iterations
        val alloc2 = after2 - before2 // 2*reps iterations
        math.max(0L, alloc2 - alloc1) // slope * reps; negative means burst-only, clamp to zero
  end measureAlloc

end AllocMeter
