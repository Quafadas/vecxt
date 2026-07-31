package vecxt.audit

/** The gate. One test per check, each failing with enough detail to act on without reading a job log — CI turns a JUnit
  * failure into an annotation against the diff, which is the only channel that reliably survives.
  */
class AuditSuite extends munit.FunSuite:

  private def result = Audited.result

  private def failures(check: String): Seq[Finding] =
    result.of(check).filter(_.severity == Severity.Fail)

  private def render(fs: Seq[Finding]): String = fs.map("  " + _.line).mkString("\n")

  private val issue = "github.com/Quafadas/vecxt/issues/105"

  test("C0 — every class in scope could be sized") {
    val fs = failures("C0-parse")
    assert(
      fs.isEmpty,
      s"the exact-size reader failed on ${fs.size} class(es), so their methods are inside no budget:\n${render(fs)}"
    )
  }

  test("C1 — no method is inside the headroom below HugeMethodLimit") {
    val fs = failures("C1")
    assert(
      fs.isEmpty,
      s"C1 ($issue): ${fs.size} method(s) approach the limit above which HotSpot never JIT compiles them:\n" +
        s"${render(fs)}\n\nLargest methods in scope:\n${Report.largest(result, 25)}\n\n${Report.sizeBands(result)}"
    )
  }

  test("C2 — @HotPath kernels fit FreqInlineSize") {
    val fs = failures("C2")
    assert(
      fs.isEmpty,
      s"C2 ($issue): ${fs.size} @HotPath method(s) are too large for C2 to inline even when hot:\n${render(fs)}\n\n" +
        s"All annotated methods:\n${Report.annotatedTable(result)}"
    )
  }

  test("C3 — @Thin forwarders fit MaxInlineSize and contain no loop") {
    val fs = failures("C3")
    assert(
      fs.isEmpty,
      s"C3 ($issue): ${fs.size} @Thin finding(s):\n${render(fs)}\n\n" +
        s"All annotated methods:\n${Report.annotatedTable(result)}"
    )
  }

  test("C6a — no ScalaRunTime specialization-failure symbols anywhere in scope") {
    val fs = failures("C6a")
    assert(
      fs.isEmpty,
      s"C6a ($issue): ${fs.size} specialization-failure hit(s). `array_length` is listed first where present — it " +
        s"identifies the method to fix, where the others identify the lines:\n${render(fs)}"
    )
  }

  test("C9 — emitted bytes per library operation has not regressed") {
    val fs = failures("C9")
    assert(
      fs.isEmpty,
      s"C9 ($issue):\n${render(fs)}\n\n${Report.ratchetTable(result)}\n\n" +
        s"Proposed baseline for bytecode/baseline.json:\n${Baseline.render(Baseline.of(result))}"
    )
  }

  test("A1 — every annotation written in source reached bytecode") {
    val fs = failures("A1")
    assert(
      fs.isEmpty,
      s"A1 ($issue): ${fs.size} annotation(s) are not enforced by anything:\n${render(fs)}\n\n" +
        s"${result.sourceAnnotations.size} annotation site(s) were found in source."
    )
  }

  test("the report was written") {
    val md = Audited.config.reportDir / "report.md"
    assert(os.exists(md), s"$md was not written")
    assert(os.size(md) > 500, s"$md is suspiciously short — the audit may have produced nothing")
  }

end AuditSuite
