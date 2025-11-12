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

import narr.*
import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.* // For apply and deepCopy

/** JVM-specific determinant implementation (currently scalar, SIMD to be added)
  */
object Determinant:

  extension (m: Matrix[Double])

    /** Calculate the determinant of a square matrix using LU decomposition
      *
      * The determinant is computed by:
      *   1. Performing LU decomposition with partial pivoting
      *   2. The determinant of L is 1 (unit lower triangular)
      *   3. The determinant of U is the product of its diagonal elements
      *   4. Accounting for row swaps (each swap multiplies det by -1)
      *
      * Time complexity: O(n³)
      *
      * @return
      *   the determinant of the matrix
      * @throws IllegalArgumentException
      *   if the matrix is not square
      */
    inline def det(using inline boundsCheck: BoundsCheck): Double =
      inline if boundsCheck then
        if m.rows != m.cols then throw new IllegalArgumentException(s"Matrix must be square, got ${m.rows}x${m.cols}")
      end if

      val n = m.rows

      // Handle small cases directly
      if n == 1 then m(0, 0)
      else if n == 2 then
        val a = m(0, 0)
        val b = m(0, 1)
        val c = m(1, 0)
        val d = m(1, 1)
        a * d - b * c
      else
        // For larger matrices, use LU decomposition with direct array access for performance
        // Create a working copy to avoid modifying the original matrix
        val lu = m.deepCopy(asRowMajor = true)
        val raw = lu.raw // Direct access to underlying array (row-major)
        var swapCount = 0

        // Perform LU decomposition with partial pivoting
        var k = 0
        var singular = false
        while k < n && !singular do
          // Find pivot - use direct array access
          var pivotRow = k
          val kRowOffset = k * n
          var maxVal = math.abs(raw(kRowOffset + k))
          var i = k + 1
          while i < n do
            val iRowOffset = i * n
            val absVal = math.abs(raw(iRowOffset + k))
            if absVal > maxVal then
              maxVal = absVal
              pivotRow = i
            end if
            i += 1
          end while

          // Check for singular matrix
          if maxVal < 1e-14 then singular = true
          else
            // Swap rows if needed - use direct array access
            if pivotRow != k then
              swapCount += 1
              val kOffset = k * n
              val pivotOffset = pivotRow * n
              var j = 0
              while j < n do
                val tmp = raw(kOffset + j)
                raw(kOffset + j) = raw(pivotOffset + j)
                raw(pivotOffset + j) = tmp
                j += 1
              end while
            end if

            // Eliminate below pivot - use direct array access with cached pivot row
            val pivotRowOffset = k * n
            val pivot = raw(pivotRowOffset + k)
            i = k + 1
            while i < n do
              val iRowOffset = i * n
              val factor = raw(iRowOffset + k) / pivot
              raw(iRowOffset + k) = factor

              // Inner loop: subtract scaled pivot row from current row
              var j = k + 1
              while j < n do
                raw(iRowOffset + j) = raw(iRowOffset + j) - factor * raw(pivotRowOffset + j)
                j += 1
              end while
              i += 1
            end while

            k += 1
          end if
        end while

        if singular then 0.0
        else
          // Compute determinant as product of diagonal elements - direct array access
          var determinant = if swapCount % 2 == 0 then 1.0 else -1.0
          var idx = 0
          while idx < n do
            determinant *= raw(idx * n + idx)
            idx += 1
          end while

          determinant
        end if
      end if
    end det

  end extension

end Determinant
