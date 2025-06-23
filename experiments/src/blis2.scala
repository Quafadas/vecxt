import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import blis.*
import vecxt.all.*
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def testBlisNative =

  println("Panama native memory test")


  val arena = Arena.ofAuto()
    // Allocate native memory for 4 doubles
  val nativeArr = arena.allocate(ValueLayout.JAVA_DOUBLE.byteSize() * 4)
  // Write values to native memory
  var i = 0
  while (i < 4) {
    nativeArr.setAtIndex(ValueLayout.JAVA_DOUBLE, i, (i + 1).toDouble)
    i += 1
  }
  // Read back and print
  i = 0
  while (i < 4) {
    val v = nativeArr.getAtIndex(ValueLayout.JAVA_DOUBLE, i)
    println(s"nativeArr($i) = $v")
    i += 1
  }