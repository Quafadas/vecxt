package vecxt

import java.io.File
import scala.jdk.CollectionConverters.*
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.MethodInsnNode
import org.objectweb.asm.tree.LineNumberNode

/** Phase 0 of the plan in https://github.com/Quafadas/vecxt/issues/105 — check C6a only, standalone and ahead of the
  * rest of the bytecode-audit infrastructure (no baseline, no annotations).
  *
  * scala/runtime/ScalaRunTime$ array accessors (and Predef$.genericWrapArray) are emitted only when a generic type
  * parameter has erased an array to Object; a numeric array library has no legitimate reason to hit any of them, so any
  * occurrence anywhere is a FAIL — no whitelist, no @HotPath scope.
  *
  * `scala/reflect/ClassTag.newArray` is deliberately left out: unlike the symbols above, it's the ordinary, non-erasure
  * way to allocate a generic array, and is only a problem inside a hot kernel — that needs the `@HotPath` scoping Phase
  * 1 introduces, so it's deferred rather than flagged here.
  *
  * `probe.*` (experiments/src/Kernel.scala, erasure.scala) is excluded from the scanned classpath below, not
  * whitelisted: those methods are deliberately-generic erasure demonstrations, not library surface, so leaving the
  * package out of scope is a decision about what counts as audited code, distinct from suppressing a specific finding.
  */
class SpecializationFailureAuditSuite extends munit.FunSuite:

  private case class Hit(file: String, line: Int, method: String, symbol: String)

  private val banned = Set(
    "scala/runtime/ScalaRunTime$" -> "array_length",
    "scala/runtime/ScalaRunTime$" -> "array_apply",
    "scala/runtime/ScalaRunTime$" -> "array_update",
    "scala/runtime/ScalaRunTime$" -> "array_clone",
    "scala/runtime/ScalaRunTime$" -> "arrayElementClass",
    "scala/runtime/ScalaRunTime$" -> "arrayClass",
    "scala/runtime/ScalaRunTime$" -> "genericArrayOps",
    "scala/Predef$" -> "genericWrapArray"
  )

  private def hitsInMethod(file: String, className: String, m: MethodNode): Seq[Hit] =
    val found = collection.mutable.ArrayBuffer.empty[Hit]
    var line = -1
    for insn <- m.instructions.asScala do
      insn match
        case ln: LineNumberNode                                      => line = ln.line
        case call: MethodInsnNode if banned((call.owner, call.name)) =>
          val symbol = s"${call.owner.replace('/', '.')}.${call.name}"
          found += Hit(file, line, s"$className.${m.name}", symbol)
        case _ => ()
    end for
    found.toSeq
  end hitsInMethod

  private def hitsInClass(bytes: Array[Byte]): Seq[Hit] =
    val node = new ClassNode(Opcodes.ASM9)
    new ClassReader(bytes).accept(node, ClassReader.SKIP_FRAMES)
    if node.name.startsWith("probe/") then Seq.empty
    else
      val file = Option(node.sourceFile).getOrElse(node.name)
      val className = node.name.replace('/', '.')
      node.methods.asScala.toSeq.flatMap(hitsInMethod(file, className, _))
    end if
  end hitsInClass

  // Every module we depend on lands its own compiled classes as a plain classpath directory (library
  // deps arrive as jars); walking the directory entries is therefore exactly "every module we own".
  private def classpathDirs: Seq[os.Path] =
    sys.props
      .getOrElse("java.class.path", "")
      .split(File.pathSeparatorChar)
      .toSeq
      .filter(_.nonEmpty)
      .flatMap(s => scala.util.Try(os.Path(s)).toOption)
      .filter(p => os.exists(p) && os.isDir(p))

  test("C6a - no ScalaRunTime specialization-failure symbols in any compiled module") {
    val hits = classpathDirs
      .flatMap(dir => os.walk(dir).filter(_.ext == "class"))
      .flatMap(f => hitsInClass(os.read.bytes(f)))
      .distinct
      .sortBy(h => (!h.symbol.endsWith(".array_length"), h.file, h.line))

    val report = hits.map(h => s"  ${h.file}:${h.line}  ${h.method}  ->  ${h.symbol}").mkString("\n")

    assert(
      hits.isEmpty,
      s"C6a (github.com/Quafadas/vecxt#105) found ${hits.size} specialization-failure hit(s):\n$report"
    )
  }

end SpecializationFailureAuditSuite
