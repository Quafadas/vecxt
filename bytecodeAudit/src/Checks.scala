package vecxt.audit

/** One `@HotPath` / `@Thin` / `@AllocFree` occurrence as written in the source.
  *
  * The audit reads annotations out of bytecode, which is the only place they can be trusted — but that means an
  * annotation which never *reached* bytecode is indistinguishable from one that was never written. Since the single
  * most likely way to write one that never reaches bytecode is to put it on an `inline def`, the source is scanned too,
  * and check A1 joins the two. See [[Checks.a1]].
  */
final case class SourceAnnotation(
    annotation: String,
    file: String,
    line: Int,
    method: String,
    isInline: Boolean
):
  def at: String = s"$file:$line"
end SourceAnnotation

object SourceAnnotation:

  val known: Seq[String] = Seq("HotPath", "Thin", "AllocFree")

  /** The convention the scan relies on, and the one every annotation in this repo is written in: the annotation starts
    * the line. A scan that tried to find annotations anywhere on a line would match its own documentation.
    */
  private val Site = ("""^@(""" + known.mkString("|") + """)\b(.*)$""").r

  private val Def = """(?:(inline)\s+)?\bdef\s+(`[^`]+`|\S+?)\s*(?:\(|\[|:|=|$)""".r

  /** Scala's operator mangling, as `scala.reflect.NameTransformer.encode` performs it. Needed because the source says
    * ``def `**!` `` and the classfile says `$times$times$bang`, and A1 compares the two.
    */
  private val operatorChars: Map[Char, String] = Map(
    '~' -> "$tilde",
    '=' -> "$eq",
    '<' -> "$less",
    '>' -> "$greater",
    '!' -> "$bang",
    '#' -> "$hash",
    '%' -> "$percent",
    '^' -> "$up",
    '&' -> "$amp",
    '|' -> "$bar",
    '*' -> "$times",
    '/' -> "$div",
    '+' -> "$plus",
    '-' -> "$minus",
    ':' -> "$colon",
    '\\' -> "$bslash",
    '?' -> "$qmark",
    '@' -> "$at"
  )

  def encode(name: String): String = name.flatMap(c => operatorChars.getOrElse(c, c.toString))

  private def isComment(line: String): Boolean =
    val t = line.trim
    t.startsWith("*") || t.startsWith("//") || t.startsWith("/*")
  end isComment

  /** The annotated `def`, looked for on the annotation's own line first and then on the following lines, skipping
    * blanks and comments. Bounded so that an annotation with no `def` after it reports as unresolved rather than
    * binding to something far below it.
    */
  private def defAfter(lines: IndexedSeq[String], start: Int, rest: String): Option[(String, Boolean)] =
    val candidates = rest +: lines.slice(start + 1, start + 9).filterNot(l => l.trim.isEmpty || isComment(l))
    candidates.iterator
      .flatMap(Def.findFirstMatchIn)
      .map(m => (m.group(2).stripPrefix("`").stripSuffix("`"), m.group(1) != null))
      .nextOption()
  end defAfter

  def scanFile(path: os.Path, display: String): Seq[SourceAnnotation] =
    val lines = os.read.lines(path)
    lines.iterator.zipWithIndex.flatMap { (raw, idx) =>
      Site.findFirstMatchIn(raw.trim).map { m =>
        // An unresolved site keeps an empty method name rather than being dropped: an annotation the scanner cannot
        // attach to a `def` is an annotation A1 would otherwise never mention.
        val (name, inlined) = defAfter(lines, idx, m.group(2)).getOrElse(("", false))
        SourceAnnotation(m.group(1), display, idx + 1, name, inlined)
      }
    }.toSeq
  end scanFile

  def scan(roots: Seq[os.Path]): Seq[SourceAnnotation] =
    roots.filter(os.exists).flatMap { root =>
      os.walk(root)
        .filter(p => os.isFile(p) && p.ext == "scala")
        .flatMap(p => scanFile(p, p.relativeTo(root).toString.replace('\\', '/')))
    }
  end scan

end SourceAnnotation

/** C9's metric: emitted bytecode of the cheatsheet, divided by the number of distinct library operations it exercises.
  *
  * The counterweight to C1/C2/C3. Those three all push in the same direction — smaller methods inline better — and left
  * alone they create a one-way ratchet towards inlining everything, which is precisely the practice
  * `site/docs/blog/2026-07-28-Inlining.md` says not to adopt. This one moves the other way: every new `inline` shows up
  * as bytes per operation going *up*, in the PR diff, at the call site that paid for it.
  *
  * Denominator is distinct `vecxt*` methods invoked from the cheatsheet's own classfiles. Note what that means under
  * pervasive inlining: an operation that is `inline` leaves no invocation behind, so it contributes bytes without
  * contributing to the count. That is not a flaw in the metric, it is the signal — inlining an operation raises the
  * ratio by construction, and a new operation with a real call raises numerator and denominator together.
  */
final case class Ratchet(sourceFile: String, totalBytes: Long, distinctOps: Int, methods: Int):
  def bytesPerOp: Double = if distinctOps == 0 then 0.0 else totalBytes.toDouble / distinctOps
end Ratchet

object Ratchet:

  val cheatsheet = "cheatsheet.scala"

  def of(methods: Seq[MethodInfo]): Ratchet =
    val mine = methods.filter(_.sourceFile == cheatsheet)
    val ops = mine
      .flatMap(_.callSites)
      .filter(_.owner.startsWith("vecxt"))
      .map(cs => (cs.owner, cs.name, cs.descriptor))
      .distinct
    Ratchet(cheatsheet, mine.map(m => math.max(m.size, 0).toLong).sum, ops.size, mine.size)
  end of

end Ratchet

object Checks:

  private def flag(m: MethodInfo, sev: Severity, check: String, msg: String): Finding =
    Finding(check, sev, m.module, m.at, m.display, msg)

  private def budget(t: Thresholds, name: String): String =
    s"$name=${t(name)} (${t.provenance(name)})"

  /** A class the size reader could not parse. FAIL, not a skip: an unreadable class is a set of methods no size check
    * ever saw, and the plan is explicit that a parse failure must never present as a pass.
    */
  def unreadable(methods: Seq[MethodInfo]): Seq[Finding] =
    methods
      .filter(_.size < 0)
      .map(m =>
        flag(
          m,
          Severity.Fail,
          "C0-parse",
          "the exact code_length could not be read for this class, so its methods are outside every size budget below"
        )
      )

  /** C1 — the hard compile cliff. Above `HugeMethodLimit`, `DontCompileHugeMethods` means the method is never JIT
    * compiled at all: it runs interpreted for the life of the process. This is the check aggressive `inline` use is
    * most likely to trip, and the only one where crossing the line costs everything rather than something.
    */
  def c1(methods: Seq[MethodInfo], t: Thresholds): Seq[Finding] =
    val huge = t("HugeMethodLimit")
    val failAt = huge * 7 / 8 // 7000 of 8000 — 12.5% headroom
    val warnAt = huge * 11 / 16 // 5500 of 8000
    methods.flatMap { m =>
      if m.size >= failAt then
        Some(
          flag(
            m,
            Severity.Fail,
            "C1",
            s"${m.size} bytes, inside the 12.5% headroom below ${budget(t, "HugeMethodLimit")}; " +
              "at the limit the method is never JIT compiled"
          )
        )
      else if m.size >= warnAt then
        Some(flag(m, Severity.Warn, "C1", s"${m.size} bytes, past 68% of ${budget(t, "HugeMethodLimit")}"))
      else None
    }
  end c1

  /** C2 — a `@HotPath` kernel above `FreqInlineSize` is not inlined into its callers however hot it gets. */
  def c2(methods: Seq[MethodInfo], t: Thresholds): Seq[Finding] =
    val freq = t("FreqInlineSize")
    val warnAt = freq * 4 / 5
    methods.filter(_.has("HotPath")).flatMap { m =>
      if m.size > freq then
        Some(
          flag(
            m,
            Severity.Fail,
            "C2",
            s"@HotPath, ${m.size} bytes, over ${budget(t, "FreqInlineSize")}: C2 will not inline it even when hot"
          )
        )
      else if m.size > warnAt then
        Some(flag(m, Severity.Warn, "C2", s"@HotPath, ${m.size} bytes, past 80% of ${budget(t, "FreqInlineSize")}"))
      else None
    }
  end c2

  /** C3 — a `@Thin` forwarder above `MaxInlineSize` stops being free at cold and lukewarm call sites, which is the only
    * reason to annotate it. A loop in one means the annotation is wrong, not the size.
    */
  def c3(methods: Seq[MethodInfo], t: Thresholds): Seq[Finding] =
    val max = t("MaxInlineSize")
    val warnAt = max - 5
    methods.filter(_.has("Thin")).flatMap { m =>
      val size =
        if m.size > max then
          Some(
            flag(
              m,
              Severity.Fail,
              "C3",
              s"@Thin, ${m.size} bytes, over ${budget(t, "MaxInlineSize")}: it no longer inlines at a cold call site"
            )
          )
        else if m.size > warnAt then
          Some(flag(m, Severity.Warn, "C3", s"@Thin, ${m.size} bytes, within 5 of ${budget(t, "MaxInlineSize")}"))
        else None
      val loop =
        if m.hasBackwardBranch then
          Some(
            flag(
              m,
              Severity.Fail,
              "C3",
              "@Thin but contains a backward branch. A loop means this method does per-element work, so " +
                "@HotPath is the annotation it wants"
            )
          )
        else None
      size.toSeq ++ loop.toSeq
    }
  end c3

  /** C6a — specialization failure. Landed in Phase 0 as a standalone munit test; unchanged in substance, moved here so
    * that the exclusion list, the report and the other checks are one thing rather than three.
    *
    * These symbols are emitted only when an abstract type parameter has erased an array to `Object`. A numeric array
    * library has no legitimate use for any of them, so there is no false-positive population to whitelist for and the
    * check is unconditional. It is also the precondition for everything else in the plan: `DoubleVector.fromArray`
    * requires a statically-typed `double[]`, so with the array erased there is no vectorised path to fall back *from*.
    *
    * Scala 3 has no `@specialized`, so the only routes to a primitive array are concrete overloads or `inline` plus
    * `compiletime.erasedValue`. That makes this check the enforcement mechanism for the "concrete type" clause of
    * `site/docs/blog/2026-07-28-Inlining.md`, in the same way C5 will be for the closure-identity clause.
    *
    * ==How to fix a finding, and two approaches that do not work==
    *
    * Both of these cost a build to establish in Phase 0 and are worth not re-attempting.
    *
    * '''Concrete sibling extension clauses cannot fix a generic arm.''' The obvious response to a finding in
    * `extension [A](x: W[A]) def m` is to add `extension (x: W[Double]) def m` beside it. That does not remove the
    * finding: it only *adds* an overload, and the generic arm survives with its erased access intact.
    *
    * They are also actively harmful. Adding a narrow clause that declares only *some* shapes of an overloaded name
    * makes every *other* shape unreachable on any receiver the narrow clause matches — Scala resolves `e.m(args)` by
    * rewriting to `m(e)`, receiver first and arguments afterwards, picks the most specific receiver, and does not fall
    * back. A narrow `extension (arr: NDArray[Double])` carrying only `apply(selectors*)` therefore breaks `arr(0)` on
    * any concretely-typed receiver. `NDArray#apply` has seven shapes across `ndarrayOps.scala` and
    * `ndarrayBooleanIndexing.scala`, so a full-coverage version is around 35 methods.
    *
    * Two things keep that survivable. It is only a hazard for an *overloaded* name, and it fails loudly — the build
    * stops compiling — so long as some call site in this repo uses an uncovered shape. Note the corollary: if none
    * does, it compiles clean here and breaks downstream users instead. `vecxt.arrayUtil.printArr` and Native's
    * `Array[A]#apply(Array[Boolean])` are the working precedent for doing it correctly: a generic clause plus concrete
    * clauses, one object, every shape covered.
    *
    * `Matrix` is structurally immune and worth keeping that way — its generic and concrete extension clauses hold
    * disjoint method *names*, so no name is ever resolved across two receiver-specificity levels. `NDArray` is the same
    * today.
    *
    * '''A production-side `inline` flag does not work either.''' The originally-planned `inline if boundsCheck == ...`
    * guard was tried and abandoned: pervasive inlining pushed method sizes past the point where C2 would compile them
    * at all. That is check C1 above, and it is what motivated bytecode analysis in the first place.
    *
    * What does work: hoist the index arithmetic (which is `Array[Int]`, never abstract) out of the per-element loop,
    * then type-test the backing array once and copy through a concrete primitive loop. `Array[AnyRef]` covers every
    * reference element type, so nine cases are exhaustive over JVM array types. `ndarrayOps.apply(selectors*)` is the
    * worked example.
    *
    * ==What this check cannot see==
    *
    * An `inline def` body is expanded into its callers rather than emitted, so a generic `inline def` is audited only
    * via whatever non-inline callers exist in scope. `ndarrayOps.toArray` has the same generic copy loop that was fixed
    * in `apply(selectors*)` and is not reported, purely because nothing generic calls it — a downstream user calling it
    * from their own generic context would pay the cost in their bytecode. Conversely `strideMatInstantiateCheck`'s
    * generic arm was reported at the non-inline call site it was inlined into, not at its own definition. [[a1]] makes
    * the same blind spot explicit for the annotations; for C6a it remains open, and closing it needs TASTy rather than
    * bytecode.
    */
  def c6a(methods: Seq[MethodInfo]): Seq[Finding] =
    val hits =
      for
        m <- methods
        cs <- m.callSites
        if bannedSymbols((cs.owner, cs.name))
      yield
        val isSentinel = cs.name == sentinel
        val note = if isSentinel then " (sentinel: the whole method is on the generic path)" else ""
        val where = if cs.line > 0 then s"${m.sourceFile}:${cs.line}" else m.sourceFile
        val finding = Finding("C6a", Severity.Fail, m.module, where, m.display, s"calls ${cs.symbol}$note")
        (!isSentinel, m.sourceFile, cs.line, finding)
    hits.distinct.sortBy((notSentinel, file, line, _) => (notSentinel, file, line)).map(_._4)
  end c6a

  /** The banned set. Emitted only when an abstract type parameter has erased an array to `Object`. */
  val bannedSymbols: Set[(String, String)] = Set(
    "scala.runtime.ScalaRunTime$" -> "array_length",
    "scala.runtime.ScalaRunTime$" -> "array_apply",
    "scala.runtime.ScalaRunTime$" -> "array_update",
    "scala.runtime.ScalaRunTime$" -> "array_clone",
    "scala.runtime.ScalaRunTime$" -> "arrayElementClass",
    "scala.runtime.ScalaRunTime$" -> "arrayClass",
    "scala.runtime.ScalaRunTime$" -> "genericArrayOps",
    "scala.Predef$" -> "genericWrapArray"
  )

  /** `array_length` is the sentinel: for a statically-known `Array[Double]` the compiler emits the one-byte
    * `arraylength` instruction, so a call to this can only mean the array arrived erased. It identifies the method to
    * fix, where the `array_apply`/`array_update` hits identify the lines, which is why it is reported first.
    */
  private val sentinel = "array_length"

  /** C9 — the inline-bloat ratchet, against the checked-in baseline. */
  def c9(r: Ratchet, baseline: Option[Baseline], t: Thresholds): Seq[Finding] =
    def where(sev: Severity, msg: String): Finding =
      Finding("C9", sev, "cheatsheet", "bytecode/baseline.json", Ratchet.cheatsheet, msg)

    baseline match
      case None =>
        Seq(
          where(
            Severity.Fail,
            "no baseline is recorded, so the ratchet is enforcing nothing. Commit the proposal below to " +
              "bytecode/baseline.json, or run ./mill bytecodeAudit.updateBaseline"
          )
        )
      case Some(b) =>
        val previous = Ratchet(r.sourceFile, b.totalBytes, b.distinctOps, 0)
        val regression =
          if previous.bytesPerOp > 0 && r.bytesPerOp > previous.bytesPerOp * 1.10 then
            Seq(
              where(
                Severity.Fail,
                f"${r.bytesPerOp}%.1f bytes per library operation, up from ${previous.bytesPerOp}%.1f " +
                  f"(${(r.bytesPerOp / previous.bytesPerOp - 1) * 100}%+.1f%%, over the 10%% ratchet). " +
                  "Either an operation grew, or something became `inline` that did not need to be"
              )
            )
          else Seq.empty
        // A JDK bump is expected to move these numbers. Saying so is the point: the diff is the interesting artefact of
        // the upgrade rather than noise to suppress, and a silent comparison across JDKs would present it as a
        // regression in the code.
        val drift =
          if b.jdkMajor != t.jdkMajor then
            Seq(
              where(
                Severity.Warn,
                s"the baseline was recorded on JDK ${b.jdkMajor} and this run is JDK ${t.jdkMajor}, so any movement " +
                  "below is partly the compiler's. Re-record it on this JDK before reading the delta as a regression"
              )
            )
          else Seq.empty
        val coverage =
          if r.distinctOps < b.distinctOps then
            Seq(
              where(
                Severity.Warn,
                s"the cheatsheet now invokes ${r.distinctOps} distinct library operations, down from " +
                  s"${b.distinctOps}. A shrinking denominator loosens this check and unaudits whatever stopped " +
                  "being called"
              )
            )
          else Seq.empty
        regression ++ drift ++ coverage
    end match
  end c9

  /** A1 — annotation integrity, and the audit's answer to the blind spot Phase 0 left open.
    *
    * An `inline def` body is expanded into its callers rather than emitted, so it has no bytecode of its own: no size
    * to measure, no annotation to read. Without this check, `@HotPath` on an `inline def` would be a comment that looks
    * like a guarantee — the check would pass because there was nothing to check. So every annotation written in source
    * must be found again in bytecode, and an annotation on an `inline def` fails by name.
    *
    * This is a decision, not just a diagnostic: `@HotPath` and `@Thin` are defined to be properties of *emitted*
    * methods. A generic `inline def` is audited only through its non-inline callers, and that is now stated where it is
    * enforced rather than left as a caveat in a PR description.
    */
  def a1(sites: Seq[SourceAnnotation], methods: Seq[MethodInfo]): Seq[Finding] =
    sites.flatMap { s =>
      def finding(sev: Severity, msg: String) =
        Finding("A1", sev, "source", s.at, s"${s.annotation} on ${s.method}", msg)

      if s.method.isEmpty then
        Some(
          finding(
            Severity.Fail,
            s"@${s.annotation} is not attached to a `def` the scanner could find in the following few lines, " +
              "so nothing links it to a method"
          )
        )
      else if s.isInline then
        Some(
          finding(
            Severity.Fail,
            s"@${s.annotation} on an `inline def`. An inline body is expanded into its callers and never emitted, so " +
              "there is no bytecode to measure and no check can see it. Either drop the `inline`, or annotate the " +
              "non-inline method the work actually lands in"
          )
        )
      else
        val encoded = SourceAnnotation.encode(s.method)
        val found = methods.exists(m => m.has(s.annotation) && (m.name == s.method || m.name == encoded))
        if found then None
        else
          Some(
            finding(
              Severity.Fail,
              s"@${s.annotation} is written here but no emitted method named '$encoded' carries it. The annotation " +
                "reached neither RuntimeVisibleAnnotations nor the audited classpath"
            )
          )
        end if
      end if
    }
  end a1

end Checks
