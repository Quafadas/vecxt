package probe

import vecxt.all.*

object ErasureProbe:

  // Case A: element type CONCRETE — the Phase 1 premise
  def concreteAccess(m: Matrix[Double], other: Matrix[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < m.numel do
      acc += m.raw(i) * other.raw(i)
      i += 1
    acc

  // Case B: element type ABSTRACT — the control. Must look different.
  def genericAccess[A](m: Matrix[A], other: Matrix[A]): Int =
    var n = 0
    var i = 0
    while i < m.numel do
      if m.raw(i) == other.raw(i) then n += 1
      i += 1
    n

  // Case C: baseline — direct array, no Matrix wrapper
  def rawArrayAccess(a: Array[Double], b: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < a.length do
      acc += a(i) * b(i)
      i += 1
    acc