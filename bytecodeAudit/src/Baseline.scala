package vecxt.audit

/** The checked-in reference the C9 ratchet compares against, plus the recorded size of every annotated method so that a
  * kernel growing towards its budget is visible in the PR diff before it crosses it.
  *
  * Two deliberate departures from §1 of the plan, both because of what this environment can and cannot verify:
  *
  *   1. **One file, not one per JDK major.** The plan keys the baseline filename by JDK major. The JDK major is
  *      recorded *inside* the file instead, and a mismatch is a WARN. This keeps a JDK bump producing exactly the
  *      reviewable diff the plan wants, without a filename that has to be guessed before the first run of a new JDK can
  *      happen.
  *   2. **Never written by CI.** [[render]] is what `./mill bytecodeAudit.updateBaseline` copies into place, and the
  *      audit prints the proposed content into its report rather than applying it. Thresholds only move in a commit
  *      somebody reviewed.
  */
final case class Baseline(jdkMajor: Int, totalBytes: Long, distinctOps: Int, annotated: Map[String, Int]):
  def bytesPerOp: Double = if distinctOps == 0 then 0.0 else totalBytes.toDouble / distinctOps
end Baseline

object Baseline:

  def annotatedSizes(methods: Seq[MethodInfo]): Map[String, Int] =
    methods.filter(_.annotations.nonEmpty).map(m => m.id -> m.size).toMap

  def of(result: AuditResult): Baseline =
    Baseline(
      jdkMajor = result.thresholds.jdkMajor,
      totalBytes = result.ratchet.totalBytes,
      distinctOps = result.ratchet.distinctOps,
      annotated = annotatedSizes(result.audited)
    )

  /** Sorted keys, one method per line — the plan's requirement that a baseline diff be readable. */
  def render(b: Baseline): String =
    val entries = b.annotated.toSeq.sortBy(_._1).map((k, v) => s"""    "$k": $v""").mkString(",\n")
    s"""{
       |  "jdkMajor": ${b.jdkMajor},
       |  "c9": { "totalBytes": ${b.totalBytes}, "distinctOps": ${b.distinctOps} },
       |  "annotated": {
       |$entries
       |  }
       |}
       |""".stripMargin
  end render

  /** `None` when the file is absent, which C9 reports as a FAIL — a ratchet with no reference enforces nothing.
    *
    * A *malformed* file throws instead. That distinction is the point: an unparseable baseline read as "absent" would
    * silently downgrade the check the moment somebody hand-edited it badly.
    */
  def read(path: os.Path): Option[Baseline] =
    if !os.exists(path) then None
    else
      val json =
        try ujson.read(os.read(path))
        catch
          case e: Exception =>
            val why = e.getMessage
            sys.error(s"$path is not readable as JSON ($why). Regenerate it with bytecodeAudit.updateBaseline")
      Some(
        Baseline(
          jdkMajor = json("jdkMajor").num.toInt,
          totalBytes = json("c9")("totalBytes").num.toLong,
          distinctOps = json("c9")("distinctOps").num.toInt,
          annotated = json("annotated").obj.map((k, v) => k -> v.num.toInt).toMap
        )
      )
    end if
  end read

end Baseline
