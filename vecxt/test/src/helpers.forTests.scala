package vecxt

import matrix.Matrix
import munit.Assertions.*
import all.*
import scala.annotation.targetName

extension [A <: AnyRef](o: A) def some: Some[A] = Some(o)
end extension

def assertVecEquals(v1: Array[Double], v2: Array[Double])(implicit loc: munit.Location): Unit =
  assert(v1.length == v2.length)
  var i: Int = 0;
  while i < v1.length do
    assertEqualsDouble(v1(i), v2(i), 1 / 1e6, clue = s"at index $i")
    i += 1
  end while
end assertVecEquals

def assertVecEquals(v1: Array[Int], v2: Array[Int])(implicit loc: munit.Location): Unit =
  var i: Int = 0;
  while i < v1.length do
    munit.Assertions.assertEquals(v1(i), v2(i), clue = s"at index $i")
    i += 1
  end while
end assertVecEquals

def assertVecEquals(v1: Array[Boolean], v2: Array[Boolean])(implicit loc: munit.Location): Unit =
  var i: Int = 0;
  while i < v1.length do
    munit.Assertions.assertEquals(v1(i), v2(i), clue = s"at index $i")
    i += 1
  end while
end assertVecEquals

def assertMatrixEquals(m1: Matrix[Double], m2: Matrix[Double])(implicit loc: munit.Location): Unit =
  import BoundsCheck.DoBoundsCheck.yes
  assertEquals(m1.rows, m2.rows)
  assertEquals(m1.cols, m2.cols)
  for i <- 0 until m1.rows do
    for j <- 0 until m1.cols do assertEqualsDouble(m1(i, j), m2(i, j), 1 / 1e6, clue = s"at row $i, col $j")
    end for
  end for
end assertMatrixEquals

@targetName("assertMatrixEqualsInt")
def assertMatrixEquals(m1: Matrix[Int], m2: Matrix[Int])(implicit loc: munit.Location): Unit =
  import BoundsCheck.DoBoundsCheck.yes
  assertEquals(m1.rows, m2.rows)
  assertEquals(m1.cols, m2.cols)
  for i <- 0 until m1.rows do
    for j <- 0 until m1.cols do assertEquals(m1(i, j), m2(i, j), clue = s"at row $i, col $j")
    end for
  end for
end assertMatrixEquals

def assertVecEquals[A](v1: Array[A], v2: Array[A])(implicit loc: munit.Location): Unit =
  var i: Int = 0;
  while i < v1.length do
    munit.Assertions.assertEquals(v1(i), v2(i), clue = s"at index $i")
    i += 1
  end while
end assertVecEquals
