package vecxt

import munit.FunSuite
import narr.*
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.dimensionExtender.DimensionExtender.Dimension.*

class Matrix3Suite extends FunSuite:

 // format: off

  /** This suite tests the creation of 3D matrices using the Matrix3 class. Below is an ASCII representation of a 3D
    * matrix:
    *
    * Layer 1:
    *  [ 1 4 7 ]
    *  [ 2 5 8 ]
    *  [ 3 6 9 ]
    *
    * Layer 2:
    *  [ 10 13 16 ]
    *  [ 11 14 17 ]
    *  [ 12 15 18 ]
    *
    * The tests ensure that the matrices are correctly constructed in column-major order.
    */

  test("Creation from depth matricies") {
    val mat1 = Matrix3.fromDepthMatricies(
      Matrix.fromRows(
        NArray(1.0, 2.0, 3.0),
        NArray(4.0, 5.0, 6.0),
        NArray(7.0, 8.0, 9.0)
      ),
      Matrix.fromRows(
        NArray(10.0, 11.0, 12.0),
        NArray(13.0, 14.0, 15.0),
        NArray(16.0, 17.0, 18.0)
      )
    )

    assertEquals(mat1.shape, (3, 3, 2))
    assertVecEquals(
      mat1.raw,
      NArray(1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0, 10.0, 13.0, 16.0, 11.0, 14.0, 17.0, 12.0, 15.0, 18.0)
    )
  }



  /**
    * This test verifies the creation of a 3D matrix from column matrices.
    * Below is an ASCII representation of the 3D matrix:
    *
    * Layer 1:
    * [ 1  10 ]
    * [ 4  13 ]
    * [ 7  16 ]

    * Layer 2:
    * [ 2  11 ]
    * [ 5  14 ]
    * [ 8  17 ]

    * Layer 3:
    * [ 3  12 ]
    * [ 6  15 ]
    * [ 9  18 ]
    *
    * The test ensures that the matrices are correctly constructed in column-major order.
    */

  test("Creation from column matricies") {
    val mat1 = Matrix3.fromColumnMatrices(
      Matrix.fromRows(
        NArray(1.0, 2.0, 3.0),
        NArray(4.0, 5.0, 6.0),
        NArray(7.0, 8.0, 9.0)
      ),
      Matrix.fromRows(
        NArray(10.0, 11.0, 12.0),
        NArray(13.0, 14.0, 15.0),
        NArray(16.0, 17.0, 18.0)
      )
    )

    assertEquals(mat1.shape, (3, 2, 3))
    println(mat1.raw)
    assertVecEquals(
      mat1.raw,
      NArray(1.0, 4.0, 7.0, 10, 13.0, 16.0 , 2.0, 5.0, 8.0, 11.0, 14.0, 17.0, 3.0, 6.0, 9.0, 12.0, 15.0, 18.0)
    )
  }


  /** This test verifies the creation of a 3D matrix from row matrices.
    *
    * Below is an ASCII representation of the 3D matrix:
    *
    * Layer 1:
      [ 1.0, 4.0, 7.0 ]
      [ 10.0 13.0 16.0 ]
    *
    * Layer 2:
      [ 2.0, 5.0, 8.0 ]
     [ 11.0, 14.0, 17.0 ]
    *
    * Layer 3:
      [ 3.0, 6.0, 9.0 ]
      [ 12.0, 15.0, 18.0 ]
    */

  test("Creation from row matrices") {
    val mat1 = Matrix3.fromRowMatrices(
      Matrix.fromRows(
        NArray(1.0, 4.0, 7.0),
        NArray(2.0, 5.0, 8.0),
        NArray(3.0, 6.0, 9.0)
      ),
      Matrix.fromRows(
        NArray(10.0, 13.0, 16.0),
        NArray(11.0, 14.0, 17.0),
        NArray(12.0, 15.0, 18.0)
      )
    )

    assertEquals(mat1.shape, (2, 3, 3))
    assertVecEquals(
      mat1.raw,
      NArray(1.0, 10.0, 4.0, 13.0, 7.0, 16.0, 2.0, 11.0, 5.0, 14.0, 8.0, 17.0, 3.0, 12.0, 6.0, 15.0, 9.0, 18.0)
    )
  }

  // format: on

end Matrix3Suite
