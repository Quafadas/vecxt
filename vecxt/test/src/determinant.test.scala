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

import munit.FunSuite
import narr.*
import all.*
import BoundsCheck.DoBoundsCheck.yes

class DeterminantSuite extends FunSuite:

  test("determinant of 1x1 matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(5.0)
    )
    assertEqualsDouble(mat.det, 5.0, 1e-10)
  }

  test("determinant of 2x2 matrix") {
    // | 3  8 |
    // | 4  6 |
    // det = 3*6 - 8*4 = 18 - 32 = -14
    val mat = Matrix.fromRows[Double](
      NArray(3.0, 8.0),
      NArray(4.0, 6.0)
    )
    assertEqualsDouble(mat.det, -14.0, 1e-10)
  }

  test("determinant of 2x2 identity matrix") {
    val mat = Matrix.eye[Double](2)
    assertEqualsDouble(mat.det, 1.0, 1e-10)
  }

  test("determinant of 3x3 identity matrix") {
    val mat = Matrix.eye[Double](3)
    assertEqualsDouble(mat.det, 1.0, 1e-10)
  }

  test("determinant of 3x3 matrix") {
    // | 6  1  1 |
    // | 4 -2  5 |
    // | 2  8  7 |
    // det = 6*(-2*7 - 5*8) - 1*(4*7 - 5*2) + 1*(4*8 - (-2)*2)
    //     = 6*(-14 - 40) - 1*(28 - 10) + 1*(32 + 4)
    //     = 6*(-54) - 1*(18) + 1*(36)
    //     = -324 - 18 + 36
    //     = -306
    val mat = Matrix.fromRows[Double](
      NArray(6.0, 1.0, 1.0),
      NArray(4.0, -2.0, 5.0),
      NArray(2.0, 8.0, 7.0)
    )
    assertEqualsDouble(mat.det, -306.0, 1e-10)
  }

  test("determinant of 3x3 singular matrix (zero determinant)") {
    // | 1  2  3 |
    // | 4  5  6 |
    // | 7  8  9 |
    // This matrix is singular (rows are linearly dependent)
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(4.0, 5.0, 6.0),
      NArray(7.0, 8.0, 9.0)
    )
    assertEqualsDouble(mat.det, 0.0, 1e-10)
  }

  test("determinant of 4x4 identity matrix") {
    val mat = Matrix.eye[Double](4)
    assertEqualsDouble(mat.det, 1.0, 1e-10)
  }

  test("determinant of 4x4 matrix") {
    // | 5  7  6  5 |
    // | 7  10 8  7 |
    // | 6  8  10 9 |
    // | 5  7  9  10|
    // Expected determinant: 1
    val mat = Matrix.fromRows[Double](
      NArray(5.0, 7.0, 6.0, 5.0),
      NArray(7.0, 10.0, 8.0, 7.0),
      NArray(6.0, 8.0, 10.0, 9.0),
      NArray(5.0, 7.0, 9.0, 10.0)
    )
    assertEqualsDouble(mat.det, 1.0, 1e-10)
  }

  test("determinant of upper triangular matrix") {
    // | 3  6  9  |
    // | 0  2  4  |
    // | 0  0  5  |
    // det = 3 * 2 * 5 = 30
    val mat = Matrix.fromRows[Double](
      NArray(3.0, 6.0, 9.0),
      NArray(0.0, 2.0, 4.0),
      NArray(0.0, 0.0, 5.0)
    )
    assertEqualsDouble(mat.det, 30.0, 1e-10)
  }

  test("determinant of lower triangular matrix") {
    // | 4  0  0  |
    // | 2  3  0  |
    // | 1  5  6  |
    // det = 4 * 3 * 6 = 72
    val mat = Matrix.fromRows[Double](
      NArray(4.0, 0.0, 0.0),
      NArray(2.0, 3.0, 0.0),
      NArray(1.0, 5.0, 6.0)
    )
    assertEqualsDouble(mat.det, 72.0, 1e-10)
  }

  test("determinant of diagonal matrix") {
    // | 2  0  0  |
    // | 0  3  0  |
    // | 0  0  4  |
    // det = 2 * 3 * 4 = 24
    val mat = Matrix.fromRows[Double](
      NArray(2.0, 0.0, 0.0),
      NArray(0.0, 3.0, 0.0),
      NArray(0.0, 0.0, 4.0)
    )
    assertEqualsDouble(mat.det, 24.0, 1e-10)
  }

  test("determinant of matrix with row of zeros") {
    // | 1  2  3  |
    // | 0  0  0  |
    // | 4  5  6  |
    // det = 0 (row of zeros)
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0),
      NArray(0.0, 0.0, 0.0),
      NArray(4.0, 5.0, 6.0)
    )
    assertEqualsDouble(mat.det, 0.0, 1e-10)
  }

  test("determinant throws exception for non-square matrix") {
    val mat = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), (2, 3))
    intercept[IllegalArgumentException] {
      mat.det
    }
  }

  test("determinant of negative values") {
    // | -2  1  |
    // |  3 -4  |
    // det = (-2)*(-4) - 1*3 = 8 - 3 = 5
    val mat = Matrix.fromRows[Double](
      NArray(-2.0, 1.0),
      NArray(3.0, -4.0)
    )
    assertEqualsDouble(mat.det, 5.0, 1e-10)
  }

  test("determinant of 5x5 matrix") {
    // A more complex case
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0, 3.0, 4.0, 5.0),
      NArray(2.0, 3.0, 4.0, 5.0, 6.0),
      NArray(3.0, 4.0, 5.0, 6.0, 7.0),
      NArray(4.0, 5.0, 6.0, 7.0, 8.0),
      NArray(5.0, 6.0, 7.0, 8.0, 9.0)
    )
    // This is a singular matrix with all rows linearly dependent
    assertEqualsDouble(mat.det, 0.0, 1e-10)
  }

end DeterminantSuite
