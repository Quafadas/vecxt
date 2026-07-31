package vecxt.audit

import scala.util.Try

/** §1.0 of https://github.com/Quafadas/vecxt/issues/105 — read C2's budgets out of the JVM rather than hardcoding them,
  * so that a JDK upgrade shows up as a threshold diff instead of the suite silently asserting last year's numbers.
  *
  * One finding here is worth stating up front, because the plan's expected-values table does not anticipate it.
  * **`HugeMethodLimit` is not discoverable on a release JVM.** It is a `develop` flag in HotSpot, as is
  * `DontCompileHugeMethods` that consumes it, and develop flags are compiled out of product builds: they are absent from
  * `-XX:+PrintFlagsFinal` and rejected on the command line. So the single sharpest cliff in the whole plan — the one
  * where a method is never JIT compiled at all — is the one number that cannot be read back from the JVM enforcing it.
  *
  * That is handled by splitting the thresholds in two. Flags a product build genuinely exposes must be *discovered*, and
  * a missing one is a FAIL: a parser that quietly matches nothing is the failure mode the plan's risk table calls out by
  * name. Flags it does not expose are *assumed*, from the HotSpot source default, and labelled as assumed everywhere
  * they appear. [[probeHugeMethodLimit]] additionally checks whether the running JVM will accept the flag on the command
  * line, which distinguishes "our parser missed it" from "this JVM does not have it" — without that, an assumption and a
  * parser bug look identical in the report.
  */
final case class Thresholds(
    discovered: Map[String, Long],
    assumed: Map[String, Long],
    jdk: String,
    jdkMajor: Int,
    vendor: String,
    hugeMethodLimitProbe: String,
    vectorLanes: Option[Int]
):

  def apply(name: String): Long =
    discovered
      .get(name)
      .orElse(assumed.get(name))
      .getOrElse(throw new NoSuchElementException(s"HotSpot threshold '$name' was neither discovered nor assumed"))

  def isAssumed(name: String): Boolean = !discovered.contains(name) && assumed.contains(name)

  def provenance(name: String): String = if isAssumed(name) then "assumed" else "discovered"

  /** Every threshold the checks use, in report order. */
  def all: Seq[(String, Long, String)] =
    Thresholds.wanted.map(name => (name, apply(name), provenance(name)))

end Thresholds

object Thresholds:

  /** The flags the audit reports, each with the HotSpot source default to fall back on. */
  val wanted: Seq[String] =
    Seq(
      "MaxTrivialSize",
      "MaxInlineSize",
      "FreqInlineSize",
      "MaxInlineLevel",
      "InlineSmallCode",
      "NodeCountInliningCutoff",
      "HugeMethodLimit"
    )

  private val sourceDefaults: Map[String, Long] = Map(
    "MaxTrivialSize" -> 6L,
    "MaxInlineSize" -> 35L,
    "FreqInlineSize" -> 325L,
    "MaxInlineLevel" -> 15L,
    "InlineSmallCode" -> 2500L,
    "NodeCountInliningCutoff" -> 18000L,
    "HugeMethodLimit" -> 8000L
  )

  /** Flags a product HotSpot build declares `product` and therefore prints. If one of these is missing, the parser is
    * broken or the JVM is not HotSpot — either way the audit must say so rather than fall back to a default.
    */
  val mustBeDiscovered: Set[String] =
    Set("MaxTrivialSize", "MaxInlineSize", "FreqInlineSize", "MaxInlineLevel", "InlineSmallCode")

  /** `<type> <name> = <value> {flags}`, with `:=` in place of `=` where the value is not the default. Flags with an
    * empty value (`ccstr ErrorFile = `) do not match and are not wanted.
    */
  private val FlagLine = """^\s*\S+\s+(\S+)\s+:?=\s*(\S+).*$""".r

  private def javaExe: os.Path =
    val home = os.Path(sys.props.getOrElse("java.home", sys.error("java.home is not set")))
    home / "bin" / (if scala.util.Properties.isWin then "java.exe" else "java")

  private def run(args: String*): Option[os.CommandResult] =
    Try(os.proc((javaExe.toString +: args)*).call(check = false, mergeErrIntoOut = true)).toOption

  def parseFlags(lines: Seq[String]): Map[String, Long] =
    lines.flatMap {
      case FlagLine(name, value) => Try(value.toLong).toOption.map(name -> _)
      case _                     => None
    }.toMap

  /** Does this JVM accept `-XX:HugeMethodLimit=`? A product build rejects it (develop flag, compiled out); a debug build
    * accepts it. Either answer is useful; the point is that the report says which, rather than presenting the assumed
    * 8000 as though it had been read from the JVM.
    */
  private def probeHugeMethodLimit(): String =
    run("-XX:HugeMethodLimit=8000", "-version") match
      case None => "could not be probed (the JVM would not start)"
      case Some(r) if r.exitCode == 0 =>
        "accepted on the command line, so this JVM is a debug build and the value above is authoritative"
      case Some(_) =>
        "rejected on the command line: a develop flag compiled out of this product build, so 8000 is taken from the " +
          "HotSpot source and cannot be confirmed against the running JVM"

  /** `DoubleVector.SPECIES_PREFERRED.length()`, reflectively so this module needs no `--add-modules` of its own.
    * Recorded, never asserted: GitHub runners vary between 4-lane AVX2 and 8-lane AVX-512, and §4 of the plan is
    * explicit that a lane count is a thing to report, not to gate on.
    */
  private def vectorLanes(): Option[Int] =
    Try {
      val species = Class.forName("jdk.incubator.vector.DoubleVector").getField("SPECIES_PREFERRED").get(null)
      // Via the interface, not `species.getClass`: the concrete species class is an implementation type in a
      // non-exported package, so reflecting on it would fail where reflecting on the exported interface does not.
      Class.forName("jdk.incubator.vector.VectorSpecies").getMethod("length").invoke(species).asInstanceOf[Int]
    }.toOption

  def discover(): Thresholds =
    val lines = run("-XX:+PrintFlagsFinal", "-version").map(_.out.lines().toSeq).getOrElse(Seq.empty)
    val flags = parseFlags(lines)
    val discovered = wanted.flatMap(n => flags.get(n).map(n -> _)).toMap
    val assumed = sourceDefaults.filterNot((n, _) => discovered.contains(n))
    Thresholds(
      discovered = discovered,
      assumed = assumed,
      jdk = sys.props.getOrElse("java.version", "unknown"),
      jdkMajor = Try(Runtime.version().feature()).getOrElse(-1),
      vendor = sys.props.getOrElse("java.vm.name", "unknown"),
      hugeMethodLimitProbe = probeHugeMethodLimit(),
      vectorLanes = vectorLanes()
    )
  end discover

end Thresholds
