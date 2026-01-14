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

import vecxt.BoundsCheck.BoundsCheck
import vecxt.MatrixHelper.*
import vecxt.MatrixInstance.*
import vecxt.matrix.Matrix

/** Shared determinant implementation for JS and Native platforms using scalar operations
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

    /** Calculate the adjugate (classical adjoint) matrix
      *
      * The adjugate is the transpose of the cofactor matrix. For a matrix A, adj(A) satisfies: A * adj(A) = det(A) * I
      *
      * This is useful for computing matrix inverses: A⁻¹ = adj(A) / det(A)
      *
      * Time complexity: O(n⁴) for general case using cofactor expansion
      *
      * @return
      *   the adjugate matrix
      * @throws IllegalArgumentException
      *   if the matrix is not square
      */
    inline def adj(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      inline if boundsCheck then
        if m.rows != m.cols then throw new IllegalArgumentException(s"Matrix must be square, got ${m.rows}x${m.cols}")
      end if

      val n = m.rows

      // Handle small cases directly
      if n == 1 then
        // Adjugate of 1x1 matrix [a] is [1]
        Matrix.fromRows[Double](Array(1.0))
      else if n == 2 then
        // For 2x2 matrix | a  b |, adjugate is | d  -b |
        //                | c  d |              |-c   a |
        val a = m(0, 0)
        val b = m(0, 1)
        val c = m(1, 0)
        val d = m(1, 1)
        Matrix.fromRows[Double](
          Array(d, -b),
          Array(-c, a)
        )
      else
        // For larger matrices, compute cofactor matrix and transpose
        val result = Matrix.zeros[Double](n, n)

        // Compute each element of the adjugate (which is C^T where C is cofactor matrix)
        // adj(A)[i,j] = cofactor(A)[j,i] = (-1)^(i+j) * det(M_ji)
        // where M_ji is the minor obtained by removing row j and column i
        var i = 0
        while i < n do
          var j = 0
          while j < n do
            // Compute cofactor[j,i] which goes into adj[i,j]
            val minor = getMinor(m, j, i, n)(using boundsCheck)
            val minorDet = minor.det
            val sign = if (i + j) % 2 == 0 then 1.0 else -1.0
            result(i, j) = sign * minorDet
            j += 1
          end while
          i += 1
        end while

        result
      end if
    end adj

    /** Helper function to compute the minor matrix by removing a specific row and column
      */
    private inline def getMinor(mat: Matrix[Double], removeRow: Int, removeCol: Int, n: Int)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[Double] =
      val minorSize = n - 1
      val minorData = Array.ofDim[Double](minorSize * minorSize)

      var srcRow = 0
      var dstRow = 0
      while srcRow < n do
        if srcRow != removeRow then
          var srcCol = 0
          var dstCol = 0
          while srcCol < n do
            if srcCol != removeCol then
              minorData(dstRow * minorSize + dstCol) = mat(srcRow, srcCol)
              dstCol += 1
            end if
            srcCol += 1
          end while
          dstRow += 1
        end if
        srcRow += 1
      end while

      Matrix(minorData, minorSize, minorSize)
    end getMinor

    /** Calculate the inverse of a square matrix
      *
      * The inverse is computed using: A⁻¹ = adj(A) / det(A)
      *
      * Time complexity: O(n⁴) using cofactor expansion
      *
      * @return
      *   the inverse matrix
      * @throws IllegalArgumentException
      *   if the matrix is not square
      * @throws ArithmeticException
      *   if the matrix is singular (determinant is zero)
      */
    inline def inv(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      inline if boundsCheck then
        if m.rows != m.cols then throw new IllegalArgumentException(s"Matrix must be square, got ${m.rows}x${m.cols}")
      end if

      val n = m.rows

      // Handle small cases directly for efficiency
      if n == 1 then
        val det = m(0, 0)
        if math.abs(det) < 1e-14 then throw new ArithmeticException("Matrix is singular (determinant is zero)")
        end if
        Matrix.fromRows[Double](Array(1.0 / det))
      else if n == 2 then
        val a = m(0, 0)
        val b = m(0, 1)
        val c = m(1, 0)
        val d = m(1, 1)
        val det = a * d - b * c
        if math.abs(det) < 1e-14 then throw new ArithmeticException("Matrix is singular (determinant is zero)")
        end if
        val invDet = 1.0 / det
        Matrix.fromRows[Double](
          Array(d * invDet, -b * invDet),
          Array(-c * invDet, a * invDet)
        )
      else
        // For larger matrices, use adj(A) / det(A)
        val det = m.det
        if math.abs(det) < 1e-14 then throw new ArithmeticException("Matrix is singular (determinant is zero)")
        end if

        val adj = m.adj
        val invDet = 1.0 / det

        // Scale adjugate by 1/det
        val result = Matrix.zeros[Double](n, n)
        var i = 0
        while i < n do
          var j = 0
          while j < n do
            result(i, j) = adj(i, j) * invDet
            j += 1
          end while
          i += 1
        end while
        result
      end if
    end inv

  end extension

end Determinant
