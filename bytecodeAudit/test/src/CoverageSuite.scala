package vecxt.audit

/** What keeps the rest of the audit honest.
  *
  * Every check here reports green over whatever it was pointed at, so the thing most likely to make the suite useless
  * is not a bug in a check — it is the scope quietly shrinking. Phase 0 discovered its classpath by walking
  * `java.class.path`, which cannot distinguish "this module is clean" from "this module was never there". The roots are
  * written down in `bytecodeAudit/package.mill` now, and this asserts they arrived.
  *
  * This is not C0 from the plan. C0 asserts every public method is *reachable from the cheatsheet's call graph*, which
  * needs a call-graph walk and lands in Phase 3. This is the cheaper precondition: the classes exist at all.
  */
class CoverageSuite extends munit.FunSuite:

  private def result = Audited.result

  test("coverage — every audited root contributed classes") {
    val classesPerModule = result.all.groupBy(_.module).map((m, ms) => m -> ms.map(_.className).distinct.size)
    val empty = Audited.config.roots.map(_.module).filterNot(m => classesPerModule.getOrElse(m, 0) > 0)
    assert(
      empty.isEmpty,
      s"these roots produced no classes, so nothing in them was checked: ${empty.mkString(", ")}. " +
        s"Roots: ${Audited.config.roots.map(r => s"${r.module}=${r.classes}").mkString(", ")}"
    )
  }

  test("coverage — the classes these checks exist for are in scope") {
    val classes = result.all.map(_.className).toSet
    // One per shape the audit has to cover: a JVM-only SIMD kernel object, a cross-platform generic container, and the
    // cheatsheet, which is the stand-in for user code and C9's entire denominator.
    val expected = Seq("vecxt.doublearrays$", "vecxt.matrix$", "vecxt.ndarray$", "CheatsheetTest$")
    val missing = expected.filterNot(classes.contains)
    assert(
      missing.isEmpty,
      s"expected classes are absent from the audited scope: ${missing.mkString(", ")}. Either a root is wrong or the " +
        s"class was renamed. ${classes.size} classes were loaded."
    )
  }

  test("coverage — the audited scope is not trivially small") {
    assert(
      result.audited.sizeIs > 200,
      s"only ${result.audited.size} methods were audited, which is too few for this codebase — the scope has collapsed"
    )
  }

  test("coverage — annotations reached bytecode at all") {
    val annotated = result.audited.filter(_.annotations.nonEmpty)
    assert(
      annotated.nonEmpty,
      "no method in the audited scope carries a vecxt.annotations annotation. Since Scala 3 dropped " +
        "ClassfileAnnotation, an annotation that is not Java-declared reaches TASTy and stops there, so this is the " +
        "assertion that catches the whole mechanism having silently become a comment. " +
        s"${result.sourceAnnotations.size} site(s) were found in source."
    )
  }

  /** The exclusions in [[Scope]] were reasoned about in Phase 0, and two of them — `mnist.scala` and
    * `pricing_fun.scala` — were never confirmed against what the code actually contains. The theory was that any hits
    * there come from third-party `inline`/macro code (scautable's CSV type inference) expanding into a script, not from
    * vecxt.
    *
    * Recording the hits turns that theory into evidence, and turns the exclusion from "we assume this is fine" into
    * "this is what is in there, and it has not changed". It also means an exclusion can no longer hide a *new* failure.
    */
  test("recorded — the C6a hits inside excluded scopes are the ones already accounted for") {
    val observed = Checks
      .c6a(result.excluded)
      .map(f => s"${f.method} -> ${f.message.takeWhile(_ != '(').trim}")
      .distinct
      .sorted

    assertEquals(
      observed,
      CoverageSuite.recordedExclusionHits,
      "the specialization-failure hits inside excluded scopes have changed. If a hit appeared, decide whether the " +
        "exclusion is still right rather than extending the list reflexively; if one disappeared, shrink the list.\n" +
        observed.map("  " + _).mkString("\n")
    )
  }

end CoverageSuite

object CoverageSuite:

  /** What the excluded scopes actually contain, from the first run — observed rather than guessed, which was the point.
    *
    * Phase 0's theory about the two unconfirmed exclusions was that any hits in them would be third-party
    * `inline`/macro code expanding into a script rather than vecxt's own. **That holds for `pricing_fun.scala` and is
    * wrong for `mnist.scala`.** pricing_fun has exactly one hit, a `genericWrapArray` consistent with scautable's CSV
    * type inference expanding in — the file contains no type parameters and no `Array[A]` of its own. mnist has four,
    * and `oneHot`, `oneHotEncode` and the two lambdas are its own generic array writes.
    *
    * That does not change the decision — mnist is one-off preprocessing glue and being slow there is fine — but it does
    * change what the exclusion means. It is now "we accept boxed array writes in this script", not "there is nothing
    * here". Which is exactly the difference between an exclusion that was reasoned about and one that was measured.
    *
    * The rest are the exclusions that were already confirmed, behaving as expected: the deliberate erasure probes, the
    * debug formatter, the CSV layer, and the plotting lambdas.
    */
  val recordedExclusionHits: Seq[String] = Seq(
    "experiments.pricing_fun$package$.pricingFun -> calls scala.Predef$.genericWrapArray",
    "mnist$package$.dataToCoords$$anonfun$1$$anonfun$1 -> calls scala.runtime.ScalaRunTime$.array_apply",
    "mnist$package$.mnist$$anonfun$1$$anonfun$2 -> calls scala.Predef$.genericWrapArray",
    "mnist$package$.oneHot -> calls scala.runtime.ScalaRunTime$.array_update",
    "mnist$package$.oneHotEncode -> calls scala.runtime.ScalaRunTime$.array_update",
    "probe.ErasureProbe$.genericAccess -> calls scala.runtime.ScalaRunTime$.array_apply",
    "probe.Kernel$.genericAccess -> calls scala.runtime.ScalaRunTime$.array_apply",
    "vecxt.arrayUtil$.printArr -> calls scala.Predef$.genericWrapArray",
    "vecxt_io.ArrayIO$.write -> calls scala.Predef$.genericWrapArray",
    "vecxt_io.MatrixIO$.loadMatrix -> calls scala.runtime.ScalaRunTime$.array_update",
    "vecxt_io.MatrixIO$.write -> calls scala.Predef$.genericWrapArray",
    "vecxt_io.MatrixIO$.write -> calls scala.runtime.ScalaRunTime$.array_apply",
    "vecxt_io.MatrixIO$.write -> calls scala.runtime.ScalaRunTime$.array_update",
    "vecxt_re.Plots$.plotHill$$anonfun$2$$anonfun$1 -> calls scala.Predef$.genericWrapArray",
    "vecxt_re.Plots$.plotIndex$$anonfun$1$$anonfun$1 -> calls scala.Predef$.genericWrapArray",
    "vecxt_re.Plots$.plotPickands$$anonfun$2$$anonfun$1 -> calls scala.Predef$.genericWrapArray",
    "vecxt_re.Plots$.plotPickandsGamma$$anonfun$2$$anonfun$1 -> calls scala.Predef$.genericWrapArray"
  )

end CoverageSuite
