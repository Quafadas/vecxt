package vecxt.audit

import java.io.File
import scala.jdk.CollectionConverters.*
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.MethodInsnNode
import org.objectweb.asm.tree.InvokeDynamicInsnNode
import org.objectweb.asm.tree.JumpInsnNode
import org.objectweb.asm.tree.LabelNode
import org.objectweb.asm.tree.LineNumberNode
import org.objectweb.asm.tree.LookupSwitchInsnNode
import org.objectweb.asm.tree.TableSwitchInsnNode

enum Severity:
  case Fail, Warn, Info
end Severity

/** One call instruction, with the line it was attributed to. Kept per-instruction rather than as a set of callees
  * because C6a's whole value is `file:line` attribution — a hit you can navigate to is a fix, a hit you can only
  * attribute to a class is a hunt.
  */
final case class CallSite(owner: String, name: String, descriptor: String, line: Int):
  def symbol: String = s"$owner.$name"
end CallSite

final case class MethodInfo(
    module: String,
    className: String,
    sourceFile: String,
    name: String,
    descriptor: String,
    line: Int,
    /** Exact `code_length`, or -1 if the class could not be read. Never silently 0. */
    size: Int,
    annotations: Set[String],
    hasBackwardBranch: Boolean,
    invokeDynamic: Int,
    callSites: Seq[CallSite]
):
  def id: String = s"$className.$name$descriptor"
  def display: String = s"$className.$name"
  def at: String = if line > 0 then s"$sourceFile:$line" else sourceFile
  def has(annotation: String): Boolean = annotations.contains(annotation)
end MethodInfo

final case class Finding(
    check: String,
    severity: Severity,
    module: String,
    at: String,
    method: String,
    message: String
):
  def line: String = s"[${severity.toString.toUpperCase}] $check  $at  $method  —  $message"
end Finding

/** Reads one classfile into [[MethodInfo]]s: ASM for structure (annotations, branches, call sites, line numbers) joined
  * to [[ClassFile]] for the exact code length ASM cannot give.
  */
object Loader:

  private val annotationPackage = "Lvecxt/annotations/"

  private def annotationsOf(m: MethodNode): Set[String] =
    val lists = Option(m.visibleAnnotations).toSeq ++ Option(m.invisibleAnnotations).toSeq
    lists.flatMap(_.asScala).map(_.desc).collect {
      case d if d.startsWith(annotationPackage) => d.stripPrefix(annotationPackage).stripSuffix(";")
    }.toSet
  end annotationsOf

  def fromBytes(module: String, bytes: Array[Byte]): Seq[MethodInfo] =
    val node = new ClassNode(Opcodes.ASM9)
    new ClassReader(bytes).accept(node, ClassReader.SKIP_FRAMES)
    val className = node.name.replace('/', '.')
    val sourceFile = Option(node.sourceFile).getOrElse(className)

    // A failure here means we cannot size the methods of this class. It is turned into a FAIL finding by
    // Checks.unreadable rather than dropped, because a class the size checks cannot see is a gap, not a pass.
    val sizes = scala.util
      .Try(ClassFile.methodSizes(bytes).map((n, d, s) => (n, d) -> s).toMap)
      .getOrElse(Map.empty[(String, String), Int])

    node.methods.asScala.toSeq
      .filter(_.instructions.size() > 0) // abstract and native methods have no Code attribute to measure
      .map { m =>
        val insns = m.instructions
        var line = -1
        var firstLine = -1
        var indy = 0
        var backward = false
        val calls = collection.mutable.ArrayBuffer.empty[CallSite]

        def isBackward(jump: org.objectweb.asm.tree.AbstractInsnNode, target: LabelNode): Boolean =
          insns.indexOf(target) < insns.indexOf(jump)

        for insn <- insns.asScala do
          insn match
            case ln: LineNumberNode =>
              line = ln.line
              if firstLine < 0 then firstLine = ln.line
            case call: MethodInsnNode =>
              calls += CallSite(call.owner.replace('/', '.'), call.name, call.desc, line)
            case _: InvokeDynamicInsnNode => indy += 1
            case j: JumpInsnNode          => if isBackward(j, j.label) then backward = true
            case s: TableSwitchInsnNode =>
              if (s.dflt +: s.labels.asScala.toSeq).exists(isBackward(s, _)) then backward = true
            case s: LookupSwitchInsnNode =>
              if (s.dflt +: s.labels.asScala.toSeq).exists(isBackward(s, _)) then backward = true
            case _ => ()
        end for

        MethodInfo(
          module = module,
          className = className,
          sourceFile = sourceFile,
          name = m.name,
          descriptor = m.desc,
          line = firstLine,
          size = sizes.getOrElse((m.name, m.desc), -1),
          annotations = annotationsOf(m),
          hasBackwardBranch = backward,
          invokeDynamic = indy,
          callSites = calls.toSeq
        )
      }
  end fromBytes

  def fromRoot(module: String, root: os.Path): Seq[MethodInfo] =
    if !os.exists(root) then Seq.empty
    else
      os.walk(root)
        .filter(p => os.isFile(p) && p.ext == "class")
        .flatMap(p => fromBytes(module, os.read.bytes(p)))
        .toSeq
  end fromRoot

end Loader

/** What counts as audited code.
  *
  * These are scope decisions, not suppressed findings, and they are the same list for every check: a method excluded
  * because it is not library surface is excluded from the size budgets for the same reason it is excluded from C6a.
  * Inherited from Phase 0, where each was agreed with the maintainer, with one narrowing (see [[excludedMethods]]).
  *
  *   - `probe.*` (`experiments/src/Kernel.scala`, `erasure.scala`) — deliberately-generic erasure demonstrations and a
  *     bytecode-bloat probe. `probe.Kernel.chain16` exists precisely to be large, so auditing its size is measuring the
  *     ruler.
  *   - `vecxt_re.Plots` — reporting and plotting, never on a fast path.
  *   - `vecxt_io.*` — CSV read/write. Slow while parsing is fine. The one way a specialization failure could leak from
  *     here into `vecxt` core — `Matrix.row`, `inline`, called generically from `MatrixIO.write` — was checked in Phase 0
  *     and does not happen.
  *   - `mnist.scala`, `pricing_fun.scala` — one-off script glue in `experiments`, not reusable surface. Unlike the
  *     others these were never confirmed, only reasoned about, so `RecordedExclusionsSuite` pins what they actually
  *     contain rather than leaving the reasoning untested.
  */
object Scope:

  val excludedClassPrefixes: Seq[String] = Seq("probe.", "vecxt_re.Plots", "vecxt_io.")

  val excludedSourceFiles: Set[String] = Set("mnist.scala", "pricing_fun.scala")

  /** `vecxt.arrayUtil` was excluded wholesale in Phase 0 for one method: `printArr`, a debug-only formatter whose
    * generic arm formats an `Array[A]`. A package-prefix exclusion would silently exempt anything added to that object
    * later, which is the opposite of what an audit is for, so it is narrowed to the method that earned it.
    */
  val excludedMethods: Set[(String, String)] = Set("vecxt.arrayUtil" -> "printArr")

  def excluded(m: MethodInfo): Boolean =
    excludedClassPrefixes.exists(m.className.startsWith)
      || excludedSourceFiles.contains(m.sourceFile)
      || excludedMethods.exists((cls, method) => m.className.startsWith(cls) && m.name == method)

  def audited(m: MethodInfo): Boolean = !excluded(m)

end Scope

final case class Root(module: String, classes: os.Path)

/** The audit's scope, handed in by the build. Passed as system properties through the test module's `forkArgs` rather
  * than discovered from `java.class.path`: the roots are then explicit and reviewable in `bytecodeAudit/package.mill`,
  * and `CoverageSuite` can assert the set is the intended one. Phase 0 walked the classpath instead, which works but
  * cannot tell "this module is clean" from "this module was never on the classpath".
  */
final case class Config(
    roots: Seq[Root],
    sourceRoots: Seq[os.Path],
    baseline: os.Path,
    reportDir: os.Path
)

object Config:

  private def prop(name: String): String =
    sys.props.getOrElse(
      name,
      sys.error(
        s"system property '$name' is not set. The audit takes its scope from the build: bytecodeAudit/package.mill " +
          "passes the roots through the test module's forkArgs, so this suite cannot run standalone."
      )
    )

  private def split(s: String): Seq[String] = s.split(File.pathSeparatorChar).toSeq.filter(_.nonEmpty)

  def fromSystemProperties(): Config =
    Config(
      roots = split(prop("vecxt.audit.roots")).map { entry =>
        val i = entry.indexOf('=')
        require(i > 0, s"malformed root '$entry', expected 'module=/path/to/classes'")
        Root(entry.take(i), os.Path(entry.drop(i + 1)))
      },
      sourceRoots = split(prop("vecxt.audit.sourceRoots")).map(os.Path(_)),
      baseline = os.Path(prop("vecxt.audit.baseline")),
      reportDir = os.Path(prop("vecxt.audit.reportDir"))
    )
  end fromSystemProperties

end Config

final case class AuditResult(
    thresholds: Thresholds,
    audited: Seq[MethodInfo],
    excluded: Seq[MethodInfo],
    sourceAnnotations: Seq[SourceAnnotation],
    ratchet: Ratchet,
    baseline: Option[Baseline],
    findings: Seq[Finding]
):
  def all: Seq[MethodInfo] = audited ++ excluded
  def failures: Seq[Finding] = findings.filter(_.severity == Severity.Fail)
  def warnings: Seq[Finding] = findings.filter(_.severity == Severity.Warn)
  def of(check: String): Seq[Finding] = findings.filter(_.check == check)
end AuditResult

object Audit:

  def run(config: Config): AuditResult =
    val thresholds = Thresholds.discover()
    val methods = config.roots.flatMap(r => Loader.fromRoot(r.module, r.classes))
    val (excluded, audited) = methods.partition(Scope.excluded)
    val sourceAnnotations = SourceAnnotation.scan(config.sourceRoots)
    val ratchet = Ratchet.of(methods)
    val baseline = Baseline.read(config.baseline)

    val findings =
      Checks.unreadable(audited) ++
        Checks.c1(audited, thresholds) ++
        Checks.c2(audited, thresholds) ++
        Checks.c3(audited, thresholds) ++
        Checks.c6a(audited) ++
        Checks.c9(ratchet, baseline, thresholds) ++
        Checks.a1(sourceAnnotations, audited)

    AuditResult(thresholds, audited, excluded, sourceAnnotations, ratchet, baseline, findings)
  end run

end Audit
