package vecxt.jit

import java.lang.management.ManagementFactory

/** Allocation measurement via {@code com.sun.management.ThreadMXBean}.
  *
  * Calls {@code getThreadAllocatedBytes} at two points within a single hot loop to determine how many heap bytes the
  * current thread allocated during the measurement window. For a correctly JIT-compiled {@code @AllocFree} kernel, the
  * {@code Vector} objects are eliminated by escape analysis and the count is zero. A {@code DoubleVector} that fails to
  * scalarize is roughly 64–128 bytes (header + lane storage), so any real allocation failure is far above the noise
  * floor.
  *
  * <h3>Why a single loop?</h3>
  *
  * The naive design — warmup loop, then measurement loop — has a critical flaw on loaded CI runners. The JNI call to
  * {@code getThreadAllocatedBytes} between the two loops triggers a JVM safepoint. At that safepoint, HotSpot can
  * deoptimise the compiled test-method frame (e.g., due to a biased-lock revocation, class redefinition, or GC
  * callback). When the measurement loop then starts, it begins in interpreted or C1 mode and runs roughly
  * {@code OSR_threshold / inner_loop_iters} iterations before C2 re-OSR-compiles the loop. For {@code variance(mode)},
  * which makes two SIMD passes over a 1024-element array, that is roughly 16–23 interpreted calls × ~82 KB of
  * unscalarised {@code DoubleVector} objects per call = 1.3–1.5 MB of spurious allocation in the measurement window,
  * regardless of how many warmup iterations preceded it.
  *
  * The single-loop design eliminates the inter-loop safepoint hazard. {@code getThreadAllocatedBytes} is called once
  * inside the already-compiled loop at iteration {@code warmup + reps} (a predictable always-not-taken branch that C2
  * treats as a one-shot side exit). By the time that branch fires, OSR compilation is long finished and the code has
  * been running in steady-state C2 for {@code reps} compiled iterations. The second call at loop exit lands outside the
  * hot path and its safepoint is harmless.
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

  /** Measures the total heap bytes allocated by {@code body} over the final {@code reps} iterations of a single
    * {@code warmup + reps + reps} iteration loop. Returns -1 if per-thread allocation tracking is not available.
    *
    * The first {@code warmup} iterations allow C2 to OSR-compile the loop and the kernel under test. The next
    * {@code reps} iterations are a steady-state settling window: the compiled loop is already hot, and any
    * compilation-related burst that happened during warmup is well behind us. The final {@code reps} iterations are
    * measured: {@code getThreadAllocatedBytes} is captured inside the compiled loop at iteration {@code warmup + reps}
    * — a one-shot conditional that C2 optimises to a cold branch — and the delta to the post-loop call yields the
    * measurement. Dividing by {@code reps} gives bytes/op.
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
        // Single loop: warmup iters (OSR compiles here), then reps settling iters, then reps measured iters.
        // getThreadAllocatedBytes is called INSIDE the compiled loop to avoid the inter-loop safepoint that
        // deoptimises the method frame and restarts the JIT-compile burst in the measurement window.
        val measureStart = warmup + reps
        val total = warmup + reps + reps
        var before = 0L
        var i = 0
        while i < total do
          if i == measureStart then before = b.getThreadAllocatedBytes(tid)
          end if
          body
          i += 1
        end while
        b.getThreadAllocatedBytes(tid) - before
  end measureAlloc

end AllocMeter
