package probe

import vecxt.all.*

/** Probe for measuring the bytecode cost of one vecxt operation, and how it accumulates in a caller.
  *
  * Each method chains a known number of element-wise operations. Subtracting consecutive sizes gives bytecodes-per-op
  * directly, and `chain16` versus HotSpot's 325 / 8000 thresholds says whether accumulation is a real problem.
  *
  * Run: mill experiments.bytecodeSizes then grep the CSV for `probe.Kernel`.
  *
  * The methods deliberately return a value derived from the result so nothing is eliminated as dead code, and they take
  * arrays as parameters so no constant folding is possible.
  */
object Kernel:

  def chain01(a: Array[Double], b: Array[Double]): Double =
    (a * b).sum

  def chain02(a: Array[Double], b: Array[Double]): Double =
    (a * b + a).sum

  def chain04(a: Array[Double], b: Array[Double]): Double =
    (a * b + a - b + a).sum

  def chain08(a: Array[Double], b: Array[Double]): Double =
    val t1 = a * b
    val t2 = t1 + a
    val t3 = t2 - b
    val t4 = t3 * a
    val t5 = t4 + b
    val t6 = t5 - a
    val t7 = t6 * b
    (t7 + a).sum
  end chain08

  def chain16(a: Array[Double], b: Array[Double]): Double =
    var t = a * b
    t = t + a; t = t - b; t = t * a; t = t + b
    t = t - a; t = t * b; t = t + a; t = t - b
    t = t * a; t = t + b; t = t - a; t = t * b
    t = t + a; t = t - b
    t.sum
  end chain16

  // --- The comparison that matters: same arithmetic, hand-written loop ---
  // If chain04 is much larger than manual04, the difference is expansion.

  def manual04(a: Array[Double], b: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < a.length do
      acc += a(i) * b(i) + a(i) - b(i) + a(i)
      i += 1
    end while
    acc
  end manual04

  // --- Erasure probe: confirms which methods genuinely need `inline` ---
  // concreteAccess should compile to checkcast + daload (cheap).
  // genericAccess should compile to ScalaRunTime.array_apply (boxes).
  // If they look the same, the probe is not discriminating — investigate
  // before drawing any conclusion.

  import vecxt.matrix.*

  def concreteAccess(m: Matrix[Double], other: Matrix[Double]): Double =
    var acc = 0.0
    var i = 0
    val n = m.numel
    while i < n do
      acc += m.raw(i) * other.raw(i)
      i += 1
    end while
    acc
  end concreteAccess

  def genericAccess[A](m: Matrix[A], other: Matrix[A]): Int =
    var count = 0
    var i = 0
    val n = m.numel
    while i < n do
      if m.raw(i) == other.raw(i) then count += 1
      end if
      i += 1
    end while
    count
  end genericAccess

  def rawArrayAccess(a: Array[Double], b: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < a.length do
      acc += a(i) * b(i)
      i += 1
    end while
    acc
  end rawArrayAccess

end Kernel
