package vecxt_io

import java.nio.file.Files

import vecxt.BoundsCheck.DoBoundsCheck.no
import vecxt.all.*

import ArrayIO.*
import MatrixIO.*

class MatrixIOSuite extends munit.FunSuite:

  private def withTempFile[A](suffix: String = ".csv")(body: os.Path => A): A =
    val tempFile = Files.createTempFile("vecxt-matrix-io-", suffix)
    try body(os.Path(tempFile.toString))
    finally Files.deleteIfExists(tempFile)
    end try
  end withTempFile

  test("write and load round-trip row-per-line csv"):
    val matrix = Matrix(Array(1.25, 4.5, -2.5, 5.125, 3.75, -6.25), (2, 3))

    withTempFile() { path =>
      matrix.write(path)

      assertEquals(os.read.lines(path).toSeq, Seq("1.25,-2.5,3.75", "4.5,5.125,-6.25"))

      val loaded = loadMatrix[Double](path)
      assertEquals(loaded.shape, matrix.shape)
      assertEquals(loaded.raw.toSeq, matrix.raw.toSeq)
    }

  test("loadMatrix respects custom separators"):
    withTempFile() { path =>
      os.write.over(path, "1.25;4.5\n-2.5;5.125\n3.75;-6.25")

      val loaded = loadMatrix[Double](path, ';')
      val expected = Array(1.25, -2.5, 3.75, 4.5, 5.125, -6.25)

      assertEquals(loaded.shape, (3, 2))
      assertEquals(loaded.raw.toSeq, expected.toSeq)
    }

  test("Simple csv"):
    val loaded = MatrixIO.fromResource[Int]("simple.csv")
    val expected = Array(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)

    assertEquals(loaded.shape, (3, 4))
    assertEquals(loaded.raw.toSeq, expected.toSeq)

  test("loadMatrix can drop header rows"):
    withTempFile() { path =>
      os.write.over(path, "a,b,c\n1,2,3\n4,5,6")

      val loaded = loadMatrix[Int](path, dropRows = 1)
      val expected = Array(1, 4, 2, 5, 3, 6)

      assertEquals(loaded.shape, (2, 3))
      assertEquals(loaded.raw.toSeq, expected.toSeq)
    }

  test("write and load round-trip array csv"):
    val array = Array(1.25, -2.5, 3.75, 5.125)

    withTempFile() { path =>
      array.write(path)

      assertEquals(os.read(path), "1.25,-2.5,3.75,5.125")

      val loaded = loadArray[Double](path)
      assertEquals(loaded.toSeq, array.toSeq)
    }

  test("loadArray respects custom separators"):
    withTempFile() { path =>
      os.write.over(path, "1;2;3;4")

      val loaded = loadArray[Int](path, ';')
      assertEquals(loaded.toSeq, Seq(1, 2, 3, 4))
    }

  test("array resource csv"):
    val loaded = ArrayIO.fromResource[Int]("simple_array.csv")
    assertEquals(loaded.toSeq, Seq(1, 2, 3, 4, 5))

  test("loadArray can drop header rows"):
    withTempFile() { path =>
      os.write.over(path, "header\n1,2,3")

      val loaded = loadArray[Int](path, dropRows = 1)
      assertEquals(loaded.toSeq, Seq(1, 2, 3))
    }

end MatrixIOSuite
