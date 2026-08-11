package vecxt

import scala.reflect.ClassTag

import vecxt.BooleanArrays.trues
import vecxt.annotations.HotPath

object IntArraysX:

  extension [A](vec: Array[A])
    /** Selects the elements of `vec` whose corresponding entry in `index` is `true`, keeping their original order.
      *
      * The result is exactly `index.trues` long — the mask selects, it does not resize or pad — so a mask that is all
      * `false` yields an empty array.
      *
      * Stays `inline`, unlike its neighbours [[select]] and [[contiguous]], and the difference is not stylistic. `A` is
      * abstract here, so `Array[A]` erases to `Object` and every one of `dimCheck(vec, index)` (which reads
      * `vec.length`), `vec(i)` and `newVec(j) = _` compiles to a `scala.runtime.ScalaRunTime$` call — `array_length`,
      * `array_apply`, `array_update` — all three of which the C6a bytecode check bans outright. Scala 3 has no
      * `@specialized`, so `inline` is what recovers a concrete `int[]`/`double[]` at the call site. This is the same
      * distinction `strideNDArrayCheck` draws in `NDArrayCheck.scala`: generic code that reaches into `Array[A]` keeps
      * `inline`; generic code that only touches `Array[Int]` does not need it.
      *
      * Consequently it carries no `@HotPath`, though its body is a per-element loop and would otherwise qualify. An
      * `inline def` is never emitted as a method of its own, so there is no bytecode for a check to measure, and
      * check A1 fails an annotation on one by name rather than letting it read as a guarantee nothing verifies. The
      * two are mutually exclusive here: the annotation would require dropping the `inline` that C6a requires.
      *
      * @param index
      *   the selection mask; must be the same length as `vec`
      * @return
      *   the selected elements, in `vec`'s order
      * @throws VectorDimensionMismatch
      *   if `index` is not the same length as `vec`
      */
    inline def mask(index: Array[Boolean])(using ct: ClassTag[A]): Array[A] =
      dimCheck(vec, index)
      val trues = index.trues
      val newVec: Array[A] = new Array[A](trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j = 1 + j
      end for
      newVec
    end mask
  end extension
  extension (arr: Array[Int])
    /** Gathers `arr` at the given positions: element `i` of the result is `arr(indicies(i))`.
      *
      * A gather, not a slice — `indicies` may be in any order and may repeat, in which case the source element is
      * duplicated. The result's length is `indicies.length`, which is unrelated to `arr.length`.
      *
      * Out-of-range positions are not validated here; an invalid index fails as an `ArrayIndexOutOfBoundsException`
      * from the read itself.
      *
      * Not `inline`: the receiver is a concrete `Array[Int]`, so the reads and writes already compile to
      * `iaload`/`iastore` with nothing for `inline` to specialise, and the body holds a loop rather than a constant to
      * fold. `LongArrays.select` is the same method over `Array[Long]` and is likewise a plain `def`.
      *
      * `@HotPath` rather than `@Thin`, on the same grounds as `intarrays.=:=`: the body carries a loop, so it does
      * per-element work and the budget that applies is `FreqInlineSize`, not `MaxInlineSize` — and `@Thin` additionally
      * forbids the backward branch this has. No `@AllocFree`: it returns a fresh array by construction.
      *
      * @param indicies
      *   the positions to read, in result order
      * @return
      *   a new array of the gathered elements
      */
    @HotPath
    def select(indicies: Array[Int]): Array[Int] =
      val len = indicies.length
      val out = Array.ofDim[Int](len)
      var i = 0
      while i < len do
        out(i) = arr(indicies(i))
        i += 1
      end while
      out
    end select

    /** True when `arr` is an ascending run of consecutive integers — every element exactly one more than the one before
      * it.
      *
      * Strictly ascending with a step of exactly one: `Array(0, 1, 2)` is contiguous, while `Array(2, 1, 0)`,
      * `Array(0, 2, 4)` and `Array(0, 0)` are not. An empty or single-element array is vacuously contiguous, since
      * there is no adjacent pair to violate the property.
      *
      * That edge case carries weight rather than being a curiosity: `Matrix.submatrix` and
      * `Matrix.apply(rowRange, colRange)` use this to decide between returning a zero-copy view and gathering into a
      * fresh array, so a single-index selection such as `mat(Array(1), ::)` takes the view path precisely because a
      * one-element array answers `true` here.
      *
      * Not `inline`, for the same reason as [[select]]: the receiver is a concrete `Array[Int]`, so there is no
      * abstract element type for `inline` to specialise away.
      *
      * The early exit is the loop condition rather than `scala.util.control.Breaks`. `breakable` takes its block
      * by-name, so the loop became a lambda and the `var`s it mutated had to be boxed into `IntRef`/`BooleanRef` to be
      * captured — an allocation on a path every `submatrix` and every `apply(rowRange, colRange)` runs, to express an
      * exit the `while` condition states directly. It also left `@HotPath` with nothing to describe: the per-element
      * work sat in the synthetic lambda, not in this method's bytecode, so the annotation would have been measuring
      * the wrong body.
      *
      * @return
      *   whether the elements ascend by exactly one throughout
      */
    @HotPath
    def contiguous: Boolean =
      var i = 1
      while i < arr.length && arr(i) == arr(i - 1) + 1 do
        i += 1
      end while
      // Reaching the end means every adjacent pair held; stopping early means one did not. Also gives the vacuous
      // `true` for length 0 and 1, where the loop never runs and `i` already sits at or past the end.
      i >= arr.length
    end contiguous
  end extension

end IntArraysX
