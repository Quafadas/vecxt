package vecxt.audit

/** `report.md`, `report.json`, `comment.md`, `method-sizes.csv` and `baseline-proposed.json`, written to
  * `out/bytecode-audit/`.
  *
  * The report's header answers the plan's sixth acceptance criterion — JDK version, the discovered C2 thresholds and
  * the detected vector lane count — so that a report can be interpreted without knowing which runner produced it. The
  * annotated-method table is deliberately *complete* rather than failures-only: a kernel at 300 of its 325-byte budget
  * has not failed anything and is the most useful line in the file.
  *
  * `method-sizes.csv` replaces what `experiments.bytecodeSizes` used to print. That task read classfiles from inside
  * the build with its own copy of the size reader; there is no reason for two of those now that one of them is a module
  * the checks and their fixtures share.
  */
object Report:

  def write(result: AuditResult, dir: os.Path): Unit =
    os.makeDir.all(dir)
    os.write.over(dir / "report.md", markdown(result))
    os.write.over(dir / "report.json", json(result))
    os.write.over(dir / "comment.md", comment(result))
    os.write.over(dir / "method-sizes.csv", csv(result))
    os.write.over(dir / "baseline-proposed.json", Baseline.render(Baseline.of(result)))
  end write

  // ---------------------------------------------------------------------------
  // Fragments, also used verbatim in assertion messages: a check that fails in
  // CI has to say enough in its own message to be actionable, because a job log
  // is not always reachable.
  // ---------------------------------------------------------------------------

  /** Every string here is assembled from a line sequence rather than a `stripMargin` block. Deliberate: `stripMargin`
    * strips the leading `|` from *interpolated* content too, so a markdown table dropped into one loses its first
    * column.
    */
  private def lines(parts: String*): String = parts.mkString("\n")

  private def table(header: String, rows: Seq[String]): String =
    val columns = header.count(_ == '|') - 1
    lines((header +: ("|" + " --- |" * columns) +: rows)*)
  end table

  def header(t: Thresholds): String =
    val lanes = t.vectorLanes
      .map(_.toString)
      .getOrElse("unavailable — jdk.incubator.vector is not on the module path")
    lines(
      s"JDK ${t.jdk} (major ${t.jdkMajor}), ${t.vendor}",
      "",
      s"Vector lanes (`DoubleVector.SPECIES_PREFERRED.length()`): $lanes",
      "",
      table("| threshold | value | provenance |", t.all.map((name, value, prov) => s"| `$name` | $value | $prov |")),
      "",
      s"`-XX:HugeMethodLimit=` was ${t.hugeMethodLimitProbe}."
    )
  end header

  def findingsTable(findings: Seq[Finding]): String =
    if findings.isEmpty then "_none_"
    else
      val rows = findings
        .sortBy(f => (f.severity.ordinal, f.check, f.at))
        .map(f => s"| ${f.severity.toString.toUpperCase} | ${f.check} | `${f.at}` | `${f.method}` | ${f.message} |")
      ("| severity | check | at | method | detail |" +: "| --- | --- | --- | --- | --- |" +: rows).mkString("\n")
  end findingsTable

  /** Every annotated method with its budget and headroom, worst headroom first. */
  def annotatedTable(result: AuditResult): String =
    val t = result.thresholds
    val annotated = result.primaryAnnotated
    if annotated.isEmpty then "_no annotated methods were found in the audited classpath_"
    else
      def budgetOf(m: MethodInfo): Option[Long] =
        if m.has("Thin") then Some(t("MaxInlineSize"))
        else if m.has("HotPath") then Some(t("FreqInlineSize"))
        else None

      val rows = annotated
        .map(m => (m, budgetOf(m)))
        .sortBy((m, b) => b.map(bb => m.size.toDouble / bb).getOrElse(-1.0))
        .reverse
        .map { (m, b) =>
          val used = b.map(bb => f"${m.size * 100.0 / bb}%.0f%% of $bb").getOrElse("n/a (Phase 2, D1)")
          val ann = m.annotations.toSeq.sorted.map("@" + _).mkString(" ")
          s"| `${m.display}` | $ann | ${m.size} | $used | ${if m.hasBackwardBranch then "yes" else "no"} | `${m.at}` |"
        }
      ("| method | annotations | bytes | budget used | loop | at |" +:
        "| --- | --- | --- | --- | --- | --- |" +: rows).mkString("\n")
    end if
  end annotatedTable

  def sizeBands(result: AuditResult): String =
    val t = result.thresholds
    val sizes = result.audited.map(_.size).filter(_ >= 0)
    def count(lo: Long, hi: Long) = sizes.count(s => s > lo && s <= hi)
    val trivial = t("MaxTrivialSize")
    val maxInline = t("MaxInlineSize")
    val freq = t("FreqInlineSize")
    val huge = t("HugeMethodLimit")
    Seq(
      f"| <= $trivial (trivial, always inlined) | ${sizes.count(_ <= trivial)}%d |",
      f"| ${trivial + 1}-$maxInline (inlinable cold) | ${count(trivial, maxInline)}%d |",
      f"| ${maxInline + 1}-$freq (inlinable when hot) | ${count(maxInline, freq)}%d |",
      f"| ${freq + 1}-$huge (not inlined) | ${count(freq, huge)}%d |",
      f"| > $huge (NEVER JIT COMPILED) | ${sizes.count(_ > huge)}%d |"
    ).prepended("| --- | --- |").prepended("| band | methods |").mkString("\n")
  end sizeBands

  def largest(result: AuditResult, n: Int): String =
    val rows = result.audited
      .sortBy(-_.size)
      .take(n)
      .map(m => s"| ${m.size} | `${m.display}` | ${m.module} | `${m.at}` |")
    ("| bytes | method | module | at |" +: "| --- | --- | --- | --- |" +: rows).mkString("\n")
  end largest

  def ratchetTable(result: AuditResult): String =
    val r = result.ratchet
    val b = result.baseline
    val absent = "—"

    def row(metric: String, now: Double, before: Option[Double], decimals: Int): String =
      val delta = before.filter(_ > 0).map(p => f"${(now / p - 1) * 100}%+.1f%%").getOrElse("n/a")
      val fmt = (d: Double) => if decimals == 0 then f"$d%.0f" else f"$d%.1f"
      s"| $metric | ${fmt(now)} | ${before.map(fmt).getOrElse(absent)} | $delta |"
    end row

    table(
      "| metric | now | baseline | delta |",
      Seq(
        s"| cheatsheet methods | ${r.methods} | $absent | $absent |",
        row("total bytes", r.totalBytes.toDouble, b.map(_.totalBytes.toDouble), 0),
        row("distinct library ops", r.distinctOps.toDouble, b.map(_.distinctOps.toDouble), 0),
        row("bytes per op", r.bytesPerOp, b.map(_.bytesPerOp), 1)
      )
    )
  end ratchetTable

  def scopeTable(result: AuditResult): String =
    val byModule = result.all.groupBy(_.module)
    val rows = byModule.toSeq.sortBy(_._1).map { (module, ms) =>
      val classes = ms.map(_.className).distinct.size
      val skipped = ms.count(Scope.excluded)
      s"| $module | $classes | ${ms.size} | $skipped |"
    }
    ("| module | classes | methods | excluded by scope |" +: "| --- | --- | --- | --- |" +: rows).mkString("\n")
  end scopeTable

  // ---------------------------------------------------------------------------

  def markdown(result: AuditResult): String =
    val proposed = Baseline.render(Baseline.of(result))
    val baselineNote = result.baseline match
      case None => "**No baseline is recorded.** Commit the proposal below to `bytecode/baseline.json`."
      case Some(b) if b.jdkMajor != result.thresholds.jdkMajor =>
        s"Baseline was recorded on JDK ${b.jdkMajor}, this run is JDK ${result.thresholds.jdkMajor}. A diff is " +
          "expected; that diff is the interesting artefact of the upgrade, not noise."
      case Some(_) => "Baseline is current."

    lines(
      "# Bytecode audit — Tier 1 (static)",
      "",
      "Phase 1 of [#105](https://github.com/Quafadas/vecxt/issues/105): C1, C2, C3 and C9, plus C6a from Phase 0",
      s"and A1 (annotation integrity). ${result.failures.size} FAIL, ${result.warnings.size} WARN.",
      "",
      "## Environment",
      "",
      header(result.thresholds),
      "",
      "## Findings",
      "",
      findingsTable(result.findings),
      "",
      "## Annotated methods",
      "",
      "`@HotPath` is checked against `FreqInlineSize`, `@Thin` against `MaxInlineSize`. `@AllocFree` carries no static",
      "check — it is Phase 2's (D1) work list.",
      "",
      annotatedTable(result),
      "",
      "## C9 — inline bloat ratchet",
      "",
      baselineNote,
      "",
      ratchetTable(result),
      "",
      "## Method sizes (audited scope)",
      "",
      sizeBands(result),
      "",
      "### Largest 25",
      "",
      largest(result, 25),
      "",
      "## Scope",
      "",
      scopeTable(result),
      "",
      s"Excluded class prefixes: ${Scope.excludedClassPrefixes.map("`" + _ + "`").mkString(", ")}.",
      s"Excluded source files: ${Scope.excludedSourceFiles.toSeq.sorted.map("`" + _ + "`").mkString(", ")}.",
      s"Excluded methods: ${Scope.excludedMethods.toSeq.sorted.map((c, m) => s"`$c.$m`").mkString(", ")}.",
      "",
      s"Source roots scanned for annotations produced ${result.sourceAnnotations.size} annotation site(s).",
      "",
      "## Proposed baseline",
      "",
      "```json",
      proposed,
      "```"
    )
  end markdown

  /** The sticky PR comment. Deltas first, because nobody reads absolutes; the tables that are long go behind
    * `<details>` so the comment stays skimmable while still carrying everything needed to act.
    */
  def comment(result: AuditResult): String =
    val verdict =
      if result.failures.nonEmpty then s":x: ${result.failures.size} FAIL, ${result.warnings.size} WARN"
      else if result.warnings.nonEmpty then s":warning: no failures, ${result.warnings.size} WARN"
      else ":white_check_mark: clean"

    lines(
      "<!-- vecxt-bytecode-audit -->",
      s"### Bytecode audit (Tier 1) — $verdict",
      "",
      header(result.thresholds),
      "",
      ratchetTable(result),
      "",
      findingsTable(result.findings),
      "",
      s"<details><summary>Annotated methods (${result.primaryAnnotated.size})</summary>",
      "",
      annotatedTable(result),
      "",
      "</details>",
      "",
      "<details><summary>Method sizes</summary>",
      "",
      sizeBands(result),
      "",
      largest(result, 25),
      "",
      "</details>",
      "",
      "<details><summary>Proposed baseline</summary>",
      "",
      "```json",
      Baseline.render(Baseline.of(result)),
      "```",
      "",
      "</details>"
    )
  end comment

  def csv(result: AuditResult): String =
    def clean(s: String) = s.replace(',', ';')
    val rows = result.all
      .sortBy(-_.size)
      .map(m =>
        s"${m.module},${clean(m.className)},${clean(m.name)},${clean(m.descriptor)},${m.size}," +
          s"${m.annotations.toSeq.sorted.mkString("|")},${Scope.audited(m)},${m.hasBackwardBranch}," +
          s"${m.invokeDynamic},${clean(m.at)}"
      )
    ("module,class,method,descriptor,bytes,annotations,audited,loop,invokedynamic,at" +: rows).mkString("\n")
  end csv

  def json(result: AuditResult): String =
    val t = result.thresholds
    ujson.write(
      ujson.Obj(
        "jdk" -> t.jdk,
        "jdkMajor" -> t.jdkMajor,
        "vendor" -> t.vendor,
        "vectorLanes" -> t.vectorLanes.map(i => ujson.Num(i.toDouble)).getOrElse(ujson.Null),
        "hugeMethodLimitProbe" -> t.hugeMethodLimitProbe,
        "thresholds" -> ujson.Obj.from(
          t.all.map((n, v, p) => n -> ujson.Obj("value" -> v.toDouble, "provenance" -> p))
        ),
        "counts" -> ujson.Obj(
          "auditedMethods" -> result.audited.size,
          "excludedMethods" -> result.excluded.size,
          "annotatedMethods" -> result.primaryAnnotated.size,
          "fail" -> result.failures.size,
          "warn" -> result.warnings.size
        ),
        "c9" -> ujson.Obj(
          "totalBytes" -> result.ratchet.totalBytes.toDouble,
          "distinctOps" -> result.ratchet.distinctOps,
          "bytesPerOp" -> result.ratchet.bytesPerOp
        ),
        "findings" -> ujson.Arr.from(
          result.findings
            .sortBy(f => (f.severity.ordinal, f.check, f.at))
            .map(f =>
              ujson.Obj(
                "check" -> f.check,
                "severity" -> f.severity.toString.toUpperCase,
                "module" -> f.module,
                "at" -> f.at,
                "method" -> f.method,
                "message" -> f.message
              )
            )
        ),
        "annotated" -> ujson.Obj.from(
          result.primaryAnnotated
            .map(m => m.id -> ujson.Obj("bytes" -> m.size, "annotations" -> ujson.Arr.from(m.annotations.toSeq.sorted)))
        )
      ),
      indent = 2
    )
  end json

end Report
