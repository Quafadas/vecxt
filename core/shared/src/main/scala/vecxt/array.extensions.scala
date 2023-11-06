/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vexct
import vexct.Limits.Limit
import vexct.Retentions.Retention

import scala.util.chaining.*

enum LossCalc:
  case Agg, Occ
end LossCalc

extension (vec: Array[Boolean])
  inline def countTrue: Int =
    var sum = 0
    for i <- 0 until vec.length do if vec(i) then sum = sum + 1
    sum
  end countTrue

  inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
    val result: Array[Boolean] = new Array[Boolean](vec.length)
    for i <- 0 until vec.length do result(i) = vec(i) && thatIdx(i)
    result
  end &&

  inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =
    val result: Array[Boolean] = new Array[Boolean](vec.length)
    for i <- 0 until vec.length do result(i) = vec(i) || thatIdx(i)
    result
  end ||

  // def copy: Array[Boolean] =
  //   val copyOfThisVector: Array[Boolean] = new Array[Boolean](vec.length)
  //   var i = 0
  //   while i < vec.length do
  //     copyOfThisVector(i) = vec(i)
  //     i = i + 1
  //   end while
  //   copyOfThisVector
  // end copy
end extension

extension (vec: Array[Double])

  def idx(index: Array[Boolean]) =
    val trues = index.countTrue
    val newVec = new Array[Double](trues)
    var j = 0
    for i <- 0 to trues do
      // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
      if index(i) then
        newVec(j) = vec(i)
        j += 1
    end for
    newVec
  end idx

  def increments: Array[Double] =
    val out = new Array[Double](vec.length)
    out(0) = vec(0)
    var i = 1
    while i < vec.length do
      out(i) = vec(i) - vec(i - 1)
      i = i + 1
    end while
    out
  end increments

  inline def stdDev: Double =
    // https://www.cuemath.com/data/standard-deviation/
    val mu = vec.mean
    val diffs_2 = vec.map(num => Math.pow(num - mu, 2))
    Math.sqrt(diffs_2.sum / (vec.length - 1))
  end stdDev

  inline def mean: Double = vec.sum / vec.length

  inline def sum: Double =
    var sum = 0.0
    var i = 0;
    while i < vec.length do
      sum = sum + vec(i)
      i = i + 1
    end while
    sum
  end sum

  def cumsum =
    var i = 1
    while i < vec.length do
      vec(i) = vec(i - 1) + vec(i)
      i = i + 1
    end while
  end cumsum

  def -(vec2: Array[Double]) =
    val out = new Array[Double](vec.length)
    var i = 0
    while i < vec.length do
      out(i) = vec(i) - vec2(i)
      i = i + 1
    end while
    out
  end -

  def -=(vec2: Array[Double]): Unit =
    var i = 0
    while i < vec.length do
      vec(i) = vec(i) - vec2(i)
      i = i + 1
    end while
  end -=

  def +(vec2: Array[Double]) =
    val out = new Array[Double](vec.length)
    var i = 0
    while i < vec.length do
      out(i) = vec(i) + vec2(i)
      i = i + 1
    end while
    out
  end +

  def +=(vec2: Array[Double]): Unit =
    var i = 0
    while i < vec.length do
      vec(i) = vec(i) + vec2(i)
      i = i + 1
    end while
  end +=

  def *=(d: Double) =
    var i = 0
    while i < vec.length do
      vec(i) = vec(i) * d
      i = i + 1
    end while
    vec
  end *=

  def *(d: Double) =
    val out = new Array[Double](vec.length)
    var i = 0
    while i < vec.length do
      out(i) = vec(i) * d
      i = i + 1
    end while
    out
  end *

  inline def <(num: Double): Array[Boolean] =
    logicalIdx((a, b) => a < b, num)

  inline def <=(num: Double): Array[Boolean] =
    logicalIdx((a, b) => a <= b, num)

  inline def >(num: Double): Array[Boolean] =
    logicalIdx((a, b) => a > b, num)

  inline def >=(num: Double): Array[Boolean] =
    logicalIdx((a, b) => a >= b, num)

  inline def logicalIdx(
      inline op: (Double, Double) => Boolean,
      inline num: Double
  ): Array[Boolean] =
    val n = vec.length
    val idx = new Array[Boolean](n)
    var i = 0
    while i < n do
      if op(vec(i), num) then idx(i) = true
      i = i + 1
    end while
    idx
  end logicalIdx

  /*

Retention and limit are known constants

In excel f(x) = min(max(x - retention, 0), limit))

The implementation takes advantage of their existence or not, to optimise the number of operations required.

   */
  inline def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
    (limitOpt, retentionOpt) match
      case (Some(limit), Some(retention)) =>
        var i = 0;
        while i < vec.length do
          val tmp = vec(i) - retention
          if tmp < 0.0 then vec(i) = 0.0
          else if tmp > limit then vec(i) = limit.toDouble
          else vec(i) = tmp
          end if
          i = i + 1
        end while

      case (None, Some(retention)) =>
        var i = 0;
        while i < vec.length do
          val tmp = vec(i) - retention
          if tmp < 0.0 then vec(i) = 0.0
          else vec(i) = tmp
          i = i + 1
        end while

      case (Some(limit), None) =>
        var i = 0;
        while i < vec.length do
          val tmp = vec(i)
          if tmp > limit then vec(i) = limit.toDouble
          else vec(i) = tmp
          i = i + 1
        end while

      case (None, None) => ()

  end reinsuranceFunction

  /*

Retention and limit are known constants

In excel f(x) = if(x < retention, 0, if(x > limit, limit, x)

   */
  inline def franchiseFunction(inline limitOpt: Option[Limit], inline retentionOpt: Option[Retention]): Unit =
    (limitOpt, retentionOpt) match
      case (None, None) => ()

      case (Some(limit), Some(retention)) =>
        var i = 0;
        val maxLim = limit.toDouble + retention.toDouble
        while i < vec.length do
          val tmp = vec(i)
          if tmp < retention then vec(i) = 0.0
          else if tmp > maxLim then vec(i) = maxLim
          else vec(i) = tmp
          end if
          i = i + 1
        end while

      case (Some(limit), None) =>
        var i = 0;
        while i < vec.length do
          val tmp = vec(i)
          if tmp > limit.toDouble then vec(i) = limit.toDouble
          else vec(i) = tmp
          end if
          i = i + 1
        end while
      case (None, Some(retention)) =>
        var i = 0;
        while i < vec.length do
          val tmp = vec(i)
          if tmp > retention.toDouble then vec(i) = tmp
          else vec(i) = 0.0
          end if
          i = i + 1
        end while
  end franchiseFunction

end extension

extension (vec: Array[Array[Double]])
  inline def horizontalSum: Array[Double] =
    val out = new Array[Double](vec.head.length)
    var i = 0
    while i < vec.head.length do
      var sum = 0.0
      var j = 0
      while j < vec.length do
        sum += vec(j)(i)
        // pprint.pprintln(s"j : $j i : $i vecij : ${vec(j)(i)}  out : ${out(i)} sum : $sum")
        j = j + 1
      end while
      out(i) = sum
      i = i + 1
    end while
    out
end extension
