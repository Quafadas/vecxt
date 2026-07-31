package canary

import vecxt.annotations.AllocFree
import vecxt.annotations.HotPath
import vecxt.annotations.Thin

/** Fixtures for the Tier 1 checks: some that must pass, some built to fail.
  *
  * The plan's acceptance criteria ask for both, and for a reason worth restating — "a check that has never been observed
  * to fail is a check that might not work". The failure mode being guarded against is not a check with a bug, it is a
  * check that quietly stops matching anything (an ASM upgrade, a Scala codegen change, an annotation that stops reaching
  * the classfile) and reports green forever.
  *
  * This module is compiled but is on no audited root, so nothing here can surface as a production finding. It is read
  * only by `CanarySuite`, which passes these classes and these sources to the same check functions the real audit calls.
  *
  * The C1 canary is not here. A method over 8000 bytes written in Scala is 800 lines of arithmetic; `CanarySuite`
  * synthesizes that classfile with ASM instead, which is exact, instant, and tests the same code path.
  */
object Canaries:

  /** Positive fixture. `@HotPath`, a real per-element loop, well inside `FreqInlineSize`. Must produce no finding at all
    * — including from C3, which does not apply, and A1, which must find it in bytecode.
    */
  @HotPath
  @AllocFree
  def wellBehavedKernel(xs: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < xs.length do
      acc += xs(i)
      i += 1
    end while
    acc
  end wellBehavedKernel

  /** Positive fixture for C3: a forwarder that really is thin. */
  @Thin
  def properlyThin(xs: Array[Double]): Double = xs(0)

  /** C3 size canary: annotated `@Thin`, far over `MaxInlineSize`. No loop, so this isolates the size rule. */
  @Thin
  def fatForwarder(xs: Array[Double]): Double =
    xs(0) + xs(1) * xs(2) - xs(3) / xs(4) + xs(5) * xs(6) - xs(7) / xs(8) +
      xs(9) + xs(10) * xs(11) - xs(12) / xs(13) + xs(14) * xs(15)
  end fatForwarder

  /** C3 loop canary: comfortably inside the size budget, but a forwarder that loops is mis-annotated whatever its size.
    * This isolates the backward-branch rule from the size rule.
    */
  @Thin
  def loopingForwarder(xs: Array[Double]): Int =
    var i = 0
    while i < xs.length do i += 1
    i
  end loopingForwarder

  /** A1 canary, and the one that closes Phase 0's open question. An `inline def` body is expanded into its callers and
    * never emitted, so this annotation reaches no classfile and no size check can see it. Without A1 it would read as a
    * guarantee and be enforced by nothing.
    */
  @HotPath
  inline def inlinedKernel(xs: Array[Double]): Double = xs(0) + xs(1)

  /** C6a canary. `Array#length` on an abstract element type compiles to `ScalaRunTime.array_length` rather than the
    * one-byte `arraylength` instruction — the sentinel that says the enclosing method is entirely on the generic path.
    */
  def erasedLength[A](xs: Array[A]): Int = xs.length

  /** C6a canary for the symbol that does the actual damage: element access through runtime type dispatch, with a box and
    * an unbox per element. `CanarySuite` asserts the reported line is this one, because `file:line` attribution is what
    * makes a C6a finding a fix rather than a hunt.
    */
  def erasedAccess[A](xs: Array[A], i: Int): A = xs(i)

end Canaries
