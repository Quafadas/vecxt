package vecxt

import scala.reflect.ClassTag
import scala.util.control.Breaks.*

import vecxt.BooleanArrays.trues

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
      * @param indicies
      *   the positions to read, in result order
      * @return
      *   a new array of the gathered elements
      */
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
      * abstract element type for `inline` to specialise away. Keeping it out of line also means the `breakable` block,
      * which implements the early exit by throwing, is emitted once rather than at every call site.
      *
      * @return
      *   whether the elements ascend by exactly one throughout
      */
    def contiguous: Boolean =
      var i = 1
      var out = true
      breakable {
        while i < arr.length do
          if arr(i) != arr(i - 1) + 1 then
            out = false
            break
          end if
          i += 1
        end while
      }
      out
    end contiguous
  end extension

end IntArraysX
