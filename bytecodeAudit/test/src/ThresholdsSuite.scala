package vecxt.audit

/** §1.0 — the budgets are read off the JVM rather than hardcoded, and this suite is what stops that from silently
  * becoming a no-op.
  *
  * The plan's risk table is explicit that a parser which quietly matches nothing is worse than no check, so there are two
  * tests: one against a committed snapshot of `-XX:+PrintFlagsFinal` output, which fails if the parser breaks, and one
  * against the running JVM, which fails if this JVM does not report what a product HotSpot build is supposed to report.
  * The snapshot alone would pass on a non-HotSpot JVM; the live one alone could not tell a parser bug from a JVM change.
  */
class ThresholdsSuite extends munit.FunSuite:

  /** Real `java -XX:+PrintFlagsFinal -version` output, trimmed to the lines that matter plus a few shapes that must not
    * break the parser: the JDK 17+ header, `:=` for a non-default value, a flag with an empty value, and a boolean.
    */
  private val sample = Seq(
    "[Global flags]",
    "   intx CICompilerCount   = 12   {product} {ergonomic}",
    "   bool DontCompileHugeMethods   = true   {product} {default}",
    "   ccstr ErrorFile   =   {product} {default}",
    "   intx FreqInlineSize   = 325   {pd product} {default}",
    "   intx InlineSmallCode   = 2500   {pd product} {default}",
    "   size_t MaxHeapSize   := 8589934592   {product} {ergonomic}",
    "   intx MaxInlineLevel   = 15   {product} {default}",
    "   intx MaxInlineSize   = 35   {product} {default}",
    "   intx MaxTrivialSize   = 6   {product} {default}",
    "   intx NodeCountInliningCutoff   = 18000   {product} {default}"
  )

  test("§1.0 — the PrintFlagsFinal parser reads the flags the checks depend on") {
    val flags = Thresholds.parseFlags(sample)
    assertEquals(flags.get("FreqInlineSize"), Some(325L))
    assertEquals(flags.get("MaxInlineSize"), Some(35L))
    assertEquals(flags.get("MaxTrivialSize"), Some(6L))
    assertEquals(flags.get("MaxInlineLevel"), Some(15L))
    assertEquals(flags.get("InlineSmallCode"), Some(2500L))
    assertEquals(flags.get("NodeCountInliningCutoff"), Some(18000L))
    // `:=` marks a value that is not the default and must parse the same way.
    assertEquals(flags.get("MaxHeapSize"), Some(8589934592L))
    // Non-numeric and empty values are skipped rather than parsed as zero.
    assertEquals(flags.get("ErrorFile"), None)
    assertEquals(flags.get("DontCompileHugeMethods"), None)
  }

  test("§1.0 — this JVM reports every threshold a product HotSpot build exposes") {
    val t = Audited.result.thresholds
    val missing = Thresholds.mustBeDiscovered.filterNot(t.discovered.contains).toSeq.sorted
    assert(
      missing.isEmpty,
      s"these are `product` flags on HotSpot and should have been read from the JVM: ${missing.mkString(", ")}. " +
        s"Discovered: ${t.discovered.toSeq.sorted.mkString(", ")}. Either the parser has broken or this is not " +
        "HotSpot; either way the audit must not fall back to a hardcoded default without saying so."
    )
  }

  test("§1.0 — HugeMethodLimit is available, and labelled honestly") {
    val t = Audited.result.thresholds
    assertEquals(t("HugeMethodLimit"), 8000L)
    // A develop flag, compiled out of product builds: absent from PrintFlagsFinal and rejected on the command line. If a
    // future JDK starts exposing it, this flips to "discovered" and the assertion below is the thing that notices.
    if t.isAssumed("HugeMethodLimit") then
      assert(
        t.hugeMethodLimitProbe.startsWith("rejected"),
        s"HugeMethodLimit was not discovered, yet the JVM ${t.hugeMethodLimitProbe}. That combination means " +
          "the parser missed a flag the JVM does have, which is a bug rather than an assumption."
      )
    else
      assert(
        t.hugeMethodLimitProbe.startsWith("accepted"),
        "HugeMethodLimit was read from the JVM but the JVM rejects it on the command line"
      )
  }

  test("§1.0 — the report header carries everything needed to interpret a run") {
    val header = Report.header(Audited.result.thresholds)
    assert(header.contains("JDK "), header)
    assert(header.contains("FreqInlineSize"), header)
    assert(header.contains("Vector lanes"), header)
  }

end ThresholdsSuite
