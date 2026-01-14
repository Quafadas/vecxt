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

import all.*

class ArrayAdditionSuite extends munit.FunSuite:
  import vecxt.BoundsCheck.DoBoundsCheck.yes

  test("array addition to columns (dim 0)"):
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0, 3.0),
      Array[Double](4.0, 5.0, 6.0),
      Array[Double](7.0, 8.0, 9.0)
    )

    val rowVector = Array[Double](10.0, 20.0, 30.0) // Should be added to each column
    mat += rowVector

    assertMatrixEquals(
      mat,
      Matrix.fromRows(
        Array[Double](11.0, 22.0, 33.0),
        Array[Double](14.0, 25.0, 36.0),
        Array[Double](17.0, 28.0, 39.0)
      )
    )

  test("array addition with dimension mismatch should throw exception"):
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0, 3.0),
      Array[Double](4.0, 5.0, 6.0)
    )

    val wrongSizeArray = Array[Double](1.0, 2.0) // Only 2 elements, but matrix has 3 columns

    intercept[AssertionError] {
      mat += wrongSizeArray
    }

  test("array addition to single row matrix"):
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0, 3.0)
    )

    val arr = Array[Double](100.0, 200.0, 300.0)
    mat += arr

    assertMatrixEquals(
      mat,
      Matrix.fromRows(
        Array[Double](101.0, 202.0, 303.0)
      )
    )

  test("SIMD-optimized array addition to columns (dense column-major)"):
    // Test the SIMD path for dense column-major matrices
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0, 3.0, 4.0),
      Array[Double](5.0, 6.0, 7.0, 8.0),
      Array[Double](9.0, 10.0, 11.0, 12.0),
      Array[Double](13.0, 14.0, 15.0, 16.0),
      Array[Double](13.0, 14.0, 15.0, 16.0)
    )

    val rowVector = Array[Double](100.0, 200.0, 300.0, 400.0)
    mat += rowVector

    assertMatrixEquals(
      mat,
      Matrix.fromRows(
        Array[Double](101.0, 202.0, 303.0, 404.0),
        Array[Double](105.0, 206.0, 307.0, 408.0),
        Array[Double](109.0, 210.0, 311.0, 412.0),
        Array[Double](113.0, 214.0, 315.0, 416.0),
        Array[Double](113.0, 214.0, 315.0, 416.0)
      )
    )

  test("SIMD-optimized array addition to columns (dense row-major)"):
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0, 3.0, 4.0),
      Array[Double](5.0, 6.0, 7.0, 8.0),
      Array[Double](9.0, 10.0, 11.0, 12.0),
      Array[Double](13.0, 14.0, 15.0, 16.0),
      Array[Double](13.0, 14.0, 15.0, 16.0)
    )

    val r2 = Array[Double](100.0, 200.0, 300.0, 400.0, 500)
    val transposeCheck = mat.transpose
    transposeCheck += r2

// Removed unnecessary debug print statements
    assertMatrixEquals(
      transposeCheck,
      Matrix.fromRows(
        Array[Double](101.0, 205.0, 309.0, 413.0, 513.0),
        Array[Double](102.0, 206.0, 310.0, 414.0, 514.0),
        Array[Double](103.0, 207.0, 311.0, 415.0, 515.0),
        Array[Double](104.0, 208.0, 312.0, 416.0, 516.0)
      )
    )
end ArrayAdditionSuite
