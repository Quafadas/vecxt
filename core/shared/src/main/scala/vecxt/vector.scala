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

package vecxt

import ai.dragonfly.math.vector.Vec
import ai.dragonfly.math.vector.Vec.*
import vecxt.idx.*

import scala.util.NotGiven

package object vector:

  object dynamic:

    extension [N <: Int](thisVector: Vec[N])(using ValueOf[N])
      transparent inline def index[M <: Int](index: Index[M])(using
          ValueOf[M]
      ) =
        dimCheck.dimensionCheck(valueOf[M], valueOf[N])
        val newLength = index.countTrue
        type D = newLength.type
        val newVec = Vec.zeros[D]
        var j = 0
        for i <- 0 until valueOf[M] do
          if index.at(i) then
            newVec(j) = thisVector(i)
            j += 1
        end for
        newVec.asInstanceOf[Vec[Int]]
    end extension

    extension [N <: Int](thisVector: Vec[N])

      def `+!`[M <: Int](thatVector: Vec[M])(using NotGiven[M =:= N]): Vec[N] =
        dimCheck.dimensionCheck(thisVector.dimension, thatVector.dimension)
        import ai.dragonfly.math.vector.Vec.+
        thisVector + thatVector.asInstanceOf[Vec[N]]
      end `+!`

      inline def <(num: Double): Index[N] =
        logicalIdx((a, b) => a < b, num)

      inline def <=(num: Double): Index[N] =
        logicalIdx((a, b) => a <= b, num)

      inline def >(num: Double): Index[N] =
        logicalIdx((a, b) => a > b, num)

      inline def >=(num: Double): Index[N] =
        logicalIdx((a, b) => a >= b, num)

      inline def logicalIdx(
          inline op: (Double, Double) => Boolean,
          inline num: Double
      ): Index[N] =
        val n = thisVector.dimension
        val idx = Index.none(n)
        var i = 0
        while i < n do
          if op(thisVector(i), num) then idx.changeAt(i, true)
          i = i + 1
        end while
        idx.asInstanceOf[Index[N]]
      end logicalIdx
    end extension

  end dynamic

  object logical:
    extension [N <: Int](thisVector: Vec[N])

      //     def `+!`[M <: Int](thatVector: Vec[M])(using NotGiven[M =:= N]): Vec[M] =
      //       dimensionCheck(thisVector.dimension, thatVector.dimension)
      //       import ai.dragonfly.math.vector.Vec.+
      //       thisVector + thatVector.asInstanceOf[Vec[N]]

      inline def <(num: Double): Index[N] =
        logicalIdx((a, b) => a < b, num)

      inline def <=(num: Double): Index[N] =
        logicalIdx((a, b) => a <= b, num)

      inline def >(num: Double): Index[N] =
        logicalIdx((a, b) => a > b, num)

      inline def >=(num: Double): Index[N] =
        logicalIdx((a, b) => a >= b, num)

      inline def logicalIdx(
          inline op: (Double, Double) => Boolean,
          inline num: Double
      ): Index[N] =
        val n = thisVector.dimension
        val idx = Index.none(n)
        var i = 0
        while i < n do
          if op(thisVector(i), num) then idx.changeAt(i, true)
          i = i + 1
        end while
        idx.asInstanceOf[Index[N]]
      end logicalIdx

    end extension

  end logical
end vector
