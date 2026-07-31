package vecxt.audit

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes

/** Fixtures for every Tier 1 check, positive and negative, run against the same functions the production audit calls.
  *
  * The plan's acceptance criteria require this and give the reason: a check that has never been observed to fail is a
  * check that might not work. The failure being guarded against is not a wrong finding, it is a check that stops matching
  * anything after an ASM bump, a Scala codegen change, or an annotation that quietly stops reaching the classfile — and
  * then reports green forever.
  */
class CanarySuite extends munit.FunSuite:

  private lazy val methods: Seq[MethodInfo] = Loader.fromRoot("canary", Audited.canary.classes)
  private lazy val sites: Seq[SourceAnnotation] = SourceAnnotation.scan(Audited.canary.sources)
  private def thresholds: Thresholds = Audited.result.thresholds

  private def named(name: String): Seq[MethodInfo] = methods.filter(_.name == name)

  private lazy val canarySource: os.Path =
    Audited.canary.sources.iterator
      .filter(os.exists)
      .flatMap(root => os.walk(root).filter(p => p.last == "canary.scala"))
      .next()

  /** The 1-based line containing `needle`. Computed rather than written down so that editing the fixture cannot make a
    * line assertion quietly wrong.
    */
  private def lineOf(needle: String): Int =
    val idx = os.read.lines(canarySource).indexWhere(_.contains(needle))
    assert(idx >= 0, s"'$needle' is not in $canarySource")
    idx + 1
  end lineOf

  /** A classfile with one method of exactly `codeBytes` bytes of code. Synthesized rather than compiled: a Scala method
    * over 8000 bytes is hundreds of lines of arithmetic, and this is exact, instant, and exercises the same reader.
    */
  private def syntheticMethod(codeBytes: Int): Array[Byte] =
    val cw = new ClassWriter(0)
    cw.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, "canary/Synthetic", null, "java/lang/Object", null)
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "grown", "()V", null, null)
    mv.visitCode()
    var i = 1
    while i < codeBytes do
      mv.visitInsn(Opcodes.NOP)
      i += 1
    end while
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    cw.visitEnd()
    cw.toByteArray()
  end syntheticMethod

  test("canary — the size reader reports the exact code_length, not a bound") {
    val ms = Loader.fromBytes("canary", syntheticMethod(1234))
    assertEquals(ms.map(m => (m.name, m.size)), Seq(("grown", 1234)))
  }

  test("canary — C1 fails above HugeMethodLimit and warns in the band below") {
    val limit = thresholds("HugeMethodLimit").toInt
    def c1(size: Int) = Checks.c1(Loader.fromBytes("canary", syntheticMethod(size)), thresholds)

    assert(c1(limit + 1).exists(_.severity == Severity.Fail), "a method over HugeMethodLimit did not FAIL C1")
    val inBand = c1(limit * 3 / 4)
    assert(
      inBand.nonEmpty && inBand.forall(_.severity == Severity.Warn),
      s"${limit * 3 / 4} bytes should WARN and only WARN, got ${inBand.map(_.line)}"
    )
    assertEquals(c1(100), Seq.empty[Finding], "a 100-byte method produced a C1 finding")
  }

  test("canary — @HotPath and @AllocFree survive to the classfile") {
    val kernel = named("wellBehavedKernel")
    assert(kernel.nonEmpty, s"the fixture is missing from ${Audited.canary.classes}")
    assert(
      kernel.exists(m => m.has("HotPath") && m.has("AllocFree")),
      "the Java-declared annotations did not reach RuntimeVisibleAnnotations on a Scala method. This is the " +
        s"assumption the whole of C2/C3 rests on. Found: ${kernel.map(m => s"${m.name}${m.annotations}")}"
    )
  }

  test("canary — a well-behaved kernel produces no finding") {
    val kernel = named("wellBehavedKernel")
    val fs = Checks.c1(kernel, thresholds) ++ Checks.c2(kernel, thresholds) ++ Checks.c3(kernel, thresholds) ++
      Checks.c6a(kernel) ++ Checks.unreadable(kernel)
    assertEquals(fs, Seq.empty[Finding], s"the positive fixture is not clean:\n${fs.map(_.line).mkString("\n")}")
    assert(kernel.exists(_.hasBackwardBranch), "the fixture should contain a loop — C2 permits one, C3 does not")
  }

  test("canary — C2 fails a @HotPath kernel over FreqInlineSize") {
    // No compiled fixture: FreqInlineSize is 325 bytes, which is a lot of Scala to write for one assertion, and the
    // annotation reaching bytecode is already established above. Synthesizing the size keeps the two facts separate.
    val kernel = named("wellBehavedKernel").filter(_.has("HotPath")).head
    val oversized = kernel.copy(size = thresholds("FreqInlineSize").toInt + 1)
    val fs = Checks.c2(Seq(oversized), thresholds)
    assert(fs.exists(_.severity == Severity.Fail), "an oversized @HotPath method did not FAIL C2")
    assert(
      Checks.c2(Seq(kernel.copy(size = (thresholds("FreqInlineSize") * 9 / 10).toInt)), thresholds)
        .forall(_.severity == Severity.Warn),
      "a @HotPath method at 90% of budget should WARN, not FAIL"
    )
  }

  test("canary — C3 fails a @Thin forwarder that is too large") {
    val fat = named("fatForwarder")
    assert(fat.exists(_.has("Thin")), s"the fixture lost its annotation: ${fat.map(_.annotations)}")
    val fs = Checks.c3(fat, thresholds)
    assert(
      fs.exists(f => f.severity == Severity.Fail && f.message.contains("MaxInlineSize")),
      s"an oversized @Thin method did not FAIL C3. Sizes: ${fat.map(m => m.name -> m.size)}, findings: " +
        fs.map(_.line).mkString("; ")
    )
  }

  test("canary — C3 fails a @Thin forwarder containing a loop, independently of its size") {
    val looping = named("loopingForwarder").filter(_.hasBackwardBranch)
    assert(looping.nonEmpty, s"the fixture has no backward branch: ${named("loopingForwarder").map(_.size)}")
    val fs = Checks.c3(looping, thresholds)
    assert(
      fs.exists(f => f.severity == Severity.Fail && f.message.contains("backward branch")),
      s"a looping @Thin method did not FAIL C3 for the loop: ${fs.map(_.line).mkString("; ")}"
    )
  }

  test("canary — C3 passes a forwarder that really is thin") {
    val thin = named("properlyThin").filter(_.has("Thin"))
    assert(thin.nonEmpty, "the fixture is missing or lost its annotation")
    assertEquals(
      Checks.c3(thin, thresholds),
      Seq.empty[Finding],
      s"the positive C3 fixture produced a finding. Sizes: ${thin.map(m => m.name -> m.size)}"
    )
  }

  test("canary — C6a fires, and attributes the hit to the right line") {
    val fs = Checks.c6a(methods)

    val accessLine = lineOf("def erasedAccess[A](xs: Array[A], i: Int): A = xs(i)")
    assert(
      fs.exists(f => f.at == s"canary.scala:$accessLine" && f.message.contains("array_apply")),
      s"expected an array_apply hit at canary.scala:$accessLine — line attribution is what makes C6a actionable. " +
        s"Got:\n${fs.map(_.line).mkString("\n")}"
    )

    val lengthLine = lineOf("def erasedLength[A](xs: Array[A]): Int = xs.length")
    assert(
      fs.exists(f => f.at == s"canary.scala:$lengthLine" && f.message.contains("array_length")),
      s"expected the array_length sentinel at canary.scala:$lengthLine. Got:\n${fs.map(_.line).mkString("\n")}"
    )

    assert(fs.head.message.contains("array_length"), s"the sentinel should sort first, got: ${fs.head.line}")
  }

  test("canary — C9 fails a regression past the ratchet and nothing inside it") {
    val base = Baseline(thresholds.jdkMajor, totalBytes = 1000L, distinctOps = 10, annotated = Map.empty)
    def ratchet(bytes: Long) = Ratchet(Ratchet.cheatsheet, bytes, 10, 1)

    assert(
      Checks.c9(ratchet(1050), Some(base), thresholds).forall(_.severity != Severity.Fail),
      "a 5% growth tripped the 10% ratchet"
    )
    assert(
      Checks.c9(ratchet(1200), Some(base), thresholds).exists(_.severity == Severity.Fail),
      "a 20% growth did not trip it"
    )
    assert(
      Checks.c9(ratchet(1000), None, thresholds).exists(_.severity == Severity.Fail),
      "a missing baseline passed — a ratchet with no reference is enforcing nothing"
    )
    assert(
      Checks.c9(Ratchet(Ratchet.cheatsheet, 1000L, 8, 1), Some(base), thresholds).exists(_.severity == Severity.Warn),
      "a shrinking denominator did not WARN — it loosens the check and unaudits whatever stopped being called"
    )
  }

  test("canary — A1 fails an annotation on an inline def") {
    val fs = Checks.a1(sites, methods)
    assert(
      fs.exists(f => f.method.contains("inlinedKernel") && f.message.contains("inline def")),
      s"@HotPath on an `inline def` was not reported. This is the blind spot Phase 0 left open: the body is never " +
        s"emitted, so without A1 the annotation is enforced by nothing. Findings:\n${fs.map(_.line).mkString("\n")}"
    )
    assert(
      !fs.exists(f => f.method.contains("wellBehavedKernel") || f.method.contains("fatForwarder")),
      s"A1 reported an annotation that did reach bytecode:\n${fs.map(_.line).mkString("\n")}"
    )
  }

  test("canary — the source scanner finds every annotation site, and reads `inline` correctly") {
    val found = sites.map(s => (s.annotation, s.method, s.isInline)).sorted
    assertEquals(
      found,
      Seq(
        ("AllocFree", "wellBehavedKernel", false),
        ("HotPath", "inlinedKernel", true),
        ("HotPath", "wellBehavedKernel", false),
        ("Thin", "fatForwarder", false),
        ("Thin", "loopingForwarder", false),
        ("Thin", "properlyThin", false)
      ),
      "the source scan drifted. A1 joins these against bytecode, so a site it misses is an annotation nothing checks."
    )
  }

  test("canary — operator names are mangled the way the compiler mangles them") {
    // A1 compares a source name against a classfile name, and `def `**!`` is `$times$times$bang` in the classfile.
    assertEquals(SourceAnnotation.encode("**!"), "$times$times$bang")
    assertEquals(SourceAnnotation.encode("clamp!"), "clamp$bang")
    assertEquals(SourceAnnotation.encode("+="), "$plus$eq")
    assertEquals(SourceAnnotation.encode("unary_-"), "unary_$minus")
    assertEquals(SourceAnnotation.encode("sumSIMD"), "sumSIMD")
  }

  test("canary — an unsizeable class is a FAIL, not a silent skip") {
    // The reader signals failure by leaving the size at -1, which is what this stands in for. What is being pinned is
    // the severity, not the parse: an unsizeable class is a set of methods that no size budget covers, and reporting it
    // as anything other than a failure is the "silently matches nothing" outcome the plan warns about.
    val ms = Loader.fromBytes("canary", syntheticMethod(50)).map(_.copy(size = -1))
    val fs = Checks.unreadable(ms)
    assert(fs.nonEmpty && fs.forall(_.severity == Severity.Fail), s"expected a FAIL, got ${fs.map(_.line)}")
  }

end CanarySuite
