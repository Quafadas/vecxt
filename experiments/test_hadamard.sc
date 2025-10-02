import vecxt.*
import vecxt.all.*
import narr.*

// Create base matrices in column-major order
val base1 = Matrix[Double](NArray.tabulate[Double](9)(i => (i + 1).toDouble), 3, 3)
val base2 = Matrix[Double](NArray.tabulate[Double](9)(i => (i + 10).toDouble), 3, 3)

println("Base1 raw: " + base1.raw.mkString(", "))
println("Base2 raw: " + base2.raw.mkString(", "))

// In column-major, indices 0,1,2 are column 0, indices 3,4,5 are column 1, indices 6,7,8 are column 2
// base1 should be: [[1,4,7], [2,5,8], [3,6,9]]
// base2 should be: [[10,13,16], [11,14,17], [12,15,18]]

// Create views with non-simple layouts - select columns 1 and 2
val view1 = base1(::, NArray(1, 2))  
val view2 = base2(::, NArray(1, 2))  

println("\nView1 elements:")
for (i <- 0 until 3; j <- 0 until 2) {
  println(s"view1($i, $j) = ${view1(i, j)}")
}

println("\nView2 elements:")
for (i <- 0 until 3; j <- 0 until 2) {
  println(s"view2($i, $j) = ${view2(i, j)}")
}
