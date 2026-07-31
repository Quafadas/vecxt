package vecxt.audit

/** Minimal JVM class file reader. Extracts the exact `code_length` of each method's Code attribute — the figure
  * HotSpot's inlining and compilation thresholds are expressed in.
  *
  * ASM is used for everything else in this module, but not for this. `CodeSizeEvaluator` only *bounds* the size (a
  * min/max pair, because re-emitting the instruction list may pick different jump widths than the compiler did), and
  * every check here compares against an exact byte budget. Reading the attribute gives the real number.
  *
  * Previously `ClassFile` in `experiments/package.mill`, backing `experiments.bytecodeSizes`. It lives in a module
  * rather than a build file now so that the same reader is used by the checks and by their fixtures — a size reader
  * that only the build can call cannot be tested.
  *
  * JVMS §4: https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html
  */
object ClassFile:

  /** (methodName, descriptor, codeLength) for every method that has code. Abstract and native methods carry no Code
    * attribute and are omitted.
    *
    * Throws on a constant pool tag it does not know. That is deliberate: a silently-skipped class is a method the size
    * checks never see, so callers turn a failure here into a FAIL finding rather than a gap.
    */
  def methodSizes(bytes: Array[Byte]): Seq[(String, String, Int)] =
    var p = 0
    inline def u1(): Int = { val v = bytes(p) & 0xff; p += 1; v }
    inline def u2(): Int = { val v = ((bytes(p) & 0xff) << 8) | (bytes(p + 1) & 0xff); p += 2; v }
    inline def u4(): Int =
      val v = ((bytes(p) & 0xff) << 24) | ((bytes(p + 1) & 0xff) << 16) |
        ((bytes(p + 2) & 0xff) << 8) | (bytes(p + 3) & 0xff)
      p += 4
      v
    inline def skip(n: Int): Unit = p += n

    require(u4() == 0xcafebabe, "not a class file")
    skip(4) // minor, major

    // --- constant pool: we only need the Utf8 entries, to resolve names ---
    val cpCount = u2()
    val utf8 = new Array[String](cpCount)
    var i = 1
    while i < cpCount do
      u1() match
        case 1 => // Utf8
          val len = u2()
          utf8(i) = new String(bytes, p, len, "UTF-8")
          skip(len)
        case 7 | 8 | 16 | 19 | 20              => skip(2)
        case 15                                => skip(3)
        case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 => skip(4)
        case 5 | 6                             => skip(8); i += 1 // Long/Double occupy two slots
        case other                             => sys.error(s"unhandled constant pool tag $other at index $i")
      i += 1
    end while

    skip(2 + 2 + 2) // access_flags, this_class, super_class
    skip(u2() * 2)  // interfaces

    def skipAttributes(): Unit =
      var n = u2()
      while n > 0 do
        skip(2)    // attribute_name_index
        skip(u4()) // attribute_length + body
        n -= 1
      end while
    end skipAttributes

    var fields = u2()
    while fields > 0 do
      skip(6) // access_flags, name_index, descriptor_index
      skipAttributes()
      fields -= 1
    end while

    val out = scala.collection.mutable.ArrayBuffer.empty[(String, String, Int)]
    var methods = u2()
    while methods > 0 do
      skip(2) // access_flags
      val name = utf8(u2())
      val desc = utf8(u2())
      var attrs = u2()
      var codeLen = -1
      while attrs > 0 do
        val attrName = utf8(u2())
        val attrLen = u4()
        if attrName == "Code" then
          skip(4) // max_stack, max_locals
          codeLen = u4()
          skip(attrLen - 8) // remainder of the Code attribute
        else skip(attrLen)
        attrs -= 1
      end while
      if codeLen >= 0 then out += ((name, desc, codeLen))
      methods -= 1
    end while

    out.toSeq
  end methodSizes

end ClassFile
