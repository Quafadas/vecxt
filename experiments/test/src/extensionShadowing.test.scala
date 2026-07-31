package vecxt

import java.io.File
import scala.annotation.targetName
import scala.compiletime.testing.typeChecks
import scala.reflect.ClassTag

/** Companion check to `specializationFailure.test.scala` (Phase 0 of https://github.com/Quafadas/vecxt/issues/105).
  *
  * The C6a fix for a specialization failure in a generic `extension [A](x: W[A])` method is always "add a
  * concretely-typed sibling clause, `extension (x: W[Double])`, so a concrete caller gets an unboxed path". Attempting
  * that for `NDArray[A]#apply(selectors*)` silently broke *unrelated, already-passing* call sites: `arr(0)` on a
  * concretely-typed `NDArray[Double]` stopped compiling once a narrower `extension (arr: NDArray[Double])` clause
  * existed that declared `apply` but not the single-`Int` shape of it.
  *
  * That is a trap the whole C6a programme will keep walking into, so it is characterised here rather than left as
  * folklore:
  *
  *   - `resolution:` tests pin down the actual Scala 3 rule with `scala.compiletime.testing.typeChecks`, which reports
  *     "does this compile" as a boolean instead of failing the build. The probes are deliberately built so that no two
  *     clauses share an erased signature, keeping the fixtures free of the `@targetName` / "clashing exports"
  *     complications that are a separate concern.
  *   - `shape-coverage:` is the actual guard. It scans vecxt's sources for the precondition of the breakage - a method
  *     name declared in both a generic-receiver and a narrower-receiver extension clause for the same wrapper type,
  *     where the narrower clause declares *fewer* overloads of that name than the generic clause does, i.e. the
  *     narrower clause fails to cover every shape the generic clause offers.
  *
  * Source scanning (not bytecode, unlike its sibling suite) is deliberate: `W[A]` and `W[Double]` erase to the same
  * descriptor, so receiver specificity - the thing that matters here - only exists at source level. It also lets the
  * check cover the JS and Native source sets, which the JVM CI leg never compiles.
  */
class ExtensionShadowingSuite extends munit.FunSuite:

  // ---------------------------------------------------------------------------
  // Part 1 - what the Scala 3 resolution rule actually is.
  //
  // `Box[A]` stands in for `NDArray[A]` / `Matrix[A]`: a wrapper whose type argument vanishes at erasure. The
  // generic clause offers two shapes of `get`; the narrow clause offers a third, taking `Long`, which exists purely
  // so that the *name* `get` is present at both specificity levels without any two definitions colliding after
  // erasure. So each probe isolates one question: does the mere existence of a narrower clause mentioning `get`
  // stop the generic clause's shapes of `get` from resolving?
  // ---------------------------------------------------------------------------

  private val genericOnly =
    typeChecks("""{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.GenericClause.*; Box(1.0).get(0) }""")

  private val genericOnlyRejectsLong =
    typeChecks("""{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.GenericClause.*; Box(1.0).get(1L) }""")

  private val sameObjectGenericShape =
    typeChecks("""{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.OneObject.*; Box(1.0).get(0) }""")

  private val sameObjectNarrowShape =
    typeChecks("""{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.OneObject.*; Box(1.0).get(1L) }""")

  private val sameObjectUnrelatedElem =
    typeChecks("""{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.OneObject.*; Box(1).get(0) }""")

  private val twoObjectsGenericShape = typeChecks(
    """{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.GenericClause.*; import vecxt.shadowProbe.NarrowClause.*; Box(1.0).get(0) }"""
  )

  private val reexportedGenericShape =
    typeChecks("""{ import vecxt.shadowProbe.Box; import vecxt.shadowProbe.Reexported.*; Box(1.0).get(0) }""")

  test("resolution: control - the probes discriminate") {
    assert(genericOnly, "generic clause alone should resolve `Box(1.0).get(0)`")
    assert(!genericOnlyRejectsLong, "generic clause alone should NOT resolve `get(1L)`; no such shape exists")
  }

  test("resolution: what a narrower clause does to the generic clause's other shapes") {
    // Measured on CI, not predicted - and the opposite of the "Scala commits to the narrowest matching clause and
    // will not fall back" story the reverted NDArray attempt was diagnosed with:
    //
    //   - Within ONE object, adding a narrow clause is harmless. Both clauses' methods are members of the same
    //     object, so this is ordinary overload resolution and the argument list does participate in it. Same when
    //     several objects are funnelled through one `export`-ing aggregator, as `vecxt.all` does.
    //   - Across TWO separate wildcard imports, the generic clause's extra shapes become unreachable. That is
    //     plain import shadowing of the name `get` - the second `import` wins and the first is never consulted -
    //     not anything specific to extension methods.
    //
    // Do NOT conclude from this that a same-object narrow clause is safe in general: the `resolution: replica`
    // test below is the same structure with realistic signatures, and it does break. These fixtures are simply
    // too simple to trigger it, which is worth keeping precisely as the boundary of what they prove.
    val actual = Seq(
      "narrow clause in the same object, generic-only shape  " -> sameObjectGenericShape,
      "narrow clause in the same object, narrow shape        " -> sameObjectNarrowShape,
      "narrow clause in the same object, other element type  " -> sameObjectUnrelatedElem,
      "narrow clause in a separate object, generic-only shape" -> twoObjectsGenericShape,
      "both re-exported via one object, generic-only shape   " -> reexportedGenericShape
    )
    val expected = Seq(
      "narrow clause in the same object, generic-only shape  " -> true,
      "narrow clause in the same object, narrow shape        " -> true,
      "narrow clause in the same object, other element type  " -> true,
      "narrow clause in a separate object, generic-only shape" -> false,
      "both re-exported via one object, generic-only shape   " -> true
    )
    assertEquals(actual, expected, "extension-method resolution no longer behaves as recorded here")
  }

  // The simplified fixtures above say a same-object narrow clause is harmless, which contradicts the reported
  // `arr(0)` breakage - so these probe a replica carrying every feature the real attempt had and `shadowProbe`
  // lacks. See `object ndarrayReplica` at the foot of this file.
  private val replicaSingleIndex =
    typeChecks("""{ import vecxt.ndarrayReplica.*; import vecxt.ndarrayReplica.Ops.*; new Nd(Array(1.0)).apply(0) }""")

  private val replicaCallSyntax = typeChecks(
    """{ import vecxt.ndarrayReplica.*; import vecxt.ndarrayReplica.Ops.*; val nd = new Nd(Array(1.0)); nd(0) }"""
  )

  private val replicaMultiIndex = typeChecks(
    """{ import vecxt.ndarrayReplica.*; import vecxt.ndarrayReplica.Ops.*; val nd = new Nd(Array(1.0)); nd(0, 0) }"""
  )

  private val replicaSelectors = typeChecks(
    """{ import vecxt.ndarrayReplica.*; import vecxt.ndarrayReplica.Ops.*; val nd = new Nd(Array(1.0)); nd(0 until 1) }"""
  )

  private val replicaUncoveredElem = typeChecks(
    """{ import vecxt.ndarrayReplica.*; import vecxt.ndarrayReplica.Ops.*; val nd = new Nd(Array("a")); nd(0) }"""
  )

  test("resolution: replica of the reverted NDArray[A]#apply attempt") {
    // Measured on CI. The replica DOES reproduce the breakage, even though the simplified `shadowProbe`
    // fixtures above - also one object, also a narrow clause - do not, so those are simply too simple to show
    // it. Adding a narrow `extension (arr: NDArray[X])` clause that declares only the selectors-vararg shape
    // makes every *other* shape of `apply` unreachable on a receiver that clause matches.
    //
    // This is why `ndarrayOps.scala:516` is fixed by manual specialization inside the one generic method
    // (a type test on the backing array) rather than by concrete sibling clauses: no narrow clause is added,
    // so none of this applies, and unlike added overloads it actually removes the generic arm's own finding.
    val actual = Seq(
      "single Int shape, explicit .apply " -> replicaSingleIndex,
      "single Int shape, nd(0) syntax    " -> replicaCallSyntax,
      "two-Int shape, nd(0, 0)           " -> replicaMultiIndex,
      "the narrow clause's vararg shape  " -> replicaSelectors,
      "element type with no narrow clause" -> replicaUncoveredElem
    )
    val expected = Seq(
      // Shapes the narrow clause does not declare become unreachable ...
      "single Int shape, explicit .apply " -> false,
      "single Int shape, nd(0) syntax    " -> false,
      "two-Int shape, nd(0, 0)           " -> false,
      // ... while the shape it does declare, and element types it does not match, are unaffected.
      "the narrow clause's vararg shape  " -> true,
      "element type with no narrow clause" -> true
    )
    assertEquals(actual, expected, "the NDArray[A]#apply shadowing replica no longer behaves as recorded here")
  }

  // ---------------------------------------------------------------------------
  // Part 2 - the guard: every narrower clause must cover every shape of the names it redeclares.
  // ---------------------------------------------------------------------------

  private case class Clause(file: os.Path, line: Int, wrapper: String, elem: String, generic: Boolean):
    def where: String = s"${file.last}:$line"
  end Clause

  /** `extension [tparams](name: Receiver)`, receiver captured up to the closing paren so a trailing comment or an
    * `inline def ...` on the same line is tolerated.
    */
  private val extRe = """^(\s*)extension\s*(\[[^\]]*\])?\s*\(\s*\w+\s*:\s*([^)]*?)\s*\)""".r

  /** Captures the method name of a `def`, including operator and backticked names. The non-greedy group stops at the
    * first `[`, `(`, or `: ` - so `def *:*(b: X)`, `def unary_- : Y`, ``def `exp!`: Unit`` and `def mapRows[B](...)`
    * all yield the name alone.
    */
  private val defRe =
    """^\s*(?:(?:transparent|inline|private|protected|final|override)\s+)*def\s+(.+?)\s*(?:\[|\(|:\s|:$|$)""".r

  private val wrapperRe = """^(\w+)\[(.*)\]$""".r

  /** Split on commas that are not nested inside brackets or parens. */
  private def topLevelSplit(s: String): Seq[String] =
    val out = collection.mutable.ArrayBuffer.empty[String]
    val sb = new StringBuilder
    var depth = 0
    s.foreach {
      case c @ ('(' | '[')   => depth += 1; sb += c
      case c @ (')' | ']')   => depth -= 1; sb += c
      case ',' if depth == 0 => out += sb.toString; sb.clear()
      case c                 => sb += c
    }
    out += sb.toString
    out.toSeq.map(_.trim).filter(_.nonEmpty)
  end topLevelSplit

  /** Names bound by a type-parameter list, e.g. `[@specialized(Double, Int) A: ClassTag, B]` -> `Set(A, B)`. */
  private def typeParamNames(tparams: String): Set[String] =
    if tparams.isEmpty then Set.empty
    else
      topLevelSplit(tparams.stripPrefix("[").stripSuffix("]"))
        .map(_.replaceAll("""@\w+\([^)]*\)""", "").trim)
        .flatMap(entry => """^([A-Za-z_]\w*)""".r.findPrefixMatchOf(entry).map(_.group(1)))
        .toSet

  /** Every `(clause, methodName)` pair declared by an extension clause whose receiver is some `Wrapper[...]`. */
  private def parse(file: os.Path): Seq[(Clause, String)] =
    val out = collection.mutable.ArrayBuffer.empty[(Clause, String)]
    var cur: Option[Clause] = None
    var clauseIndent = 0

    os.read.lines(file).zipWithIndex.foreach { case (raw, idx) =>
      val trimmed = raw.trim
      if trimmed.nonEmpty && !trimmed.startsWith("//") then
        val indent = raw.takeWhile(_ == ' ').length
        extRe.findPrefixMatchOf(raw) match
          case Some(m) =>
            clauseIndent = m.group(1).length
            cur = m.group(3) match
              case wrapperRe(wrapper, args) =>
                val tpNames = typeParamNames(Option(m.group(2)).getOrElse(""))
                val isGeneric = tpNames.exists(t => s"""\\b$t\\b""".r.findFirstIn(args).isDefined)
                Some(Clause(file, idx + 1, wrapper, if isGeneric then "A" else args, isGeneric))
              case _ => None
            // `extension [A](x: W[A]) inline def foo: T = ...` - the member sits on the clause's own line.
            val rest = raw.substring(m.end).trim
            cur.foreach(c => defRe.findPrefixMatchOf(rest).foreach(d => out += ((c, d.group(1)))))

          case None =>
            // Significant indentation: the clause ends at the first line indented no further than the clause
            // itself (which also covers `end extension`). Members sit exactly one step in; anything deeper is a
            // local def inside a method body.
            if indent <= clauseIndent then cur = None
            else if indent == clauseIndent + 2 then
              cur.foreach(c => defRe.findPrefixMatchOf(raw).foreach(d => out += ((c, d.group(1)))))
        end match
      end if
    }
    out.toSeq
  end parse

  /** Repo root, found by walking up from a compiled-classes directory on the test classpath until `build.mill`. */
  private lazy val repoRoot: Option[os.Path] =
    sys.props
      .getOrElse("java.class.path", "")
      .split(File.pathSeparatorChar)
      .toSeq
      .filter(_.nonEmpty)
      .flatMap(s => scala.util.Try(os.Path(s)).toOption)
      .filter(p => os.exists(p) && os.isDir(p))
      .flatMap { p =>
        Iterator.iterate(p)(_ / os.up).takeWhile(_.segmentCount > 0).find(d => os.exists(d / "build.mill"))
      }
      .headOption

  /** Source sets a single compilation sees, per cross-platform target. */
  private val platforms = Seq(
    "js" -> Seq("src", "src-js", "src-js-native"),
    "jvm" -> Seq("src", "src-jvm", "src-jvm-native"),
    "native" -> Seq("src", "src-native", "src-js-native", "src-jvm-native")
  )

  test("shape-coverage: concrete extension clauses cover every shape of the names they redeclare") {
    assume(repoRoot.isDefined, "could not locate the repo root from the classpath; skipping source scan")
    val root = repoRoot.get

    val problems = collection.mutable.ArrayBuffer.empty[String]
    val notes = collection.mutable.ArrayBuffer.empty[String]

    platforms.foreach { case (platform, dirs) =>
      val files = dirs.map(root / "vecxt" / _).filter(os.exists).flatMap(d => os.list(d).filter(_.ext == "scala"))
      val decls = files.sorted.flatMap(parse)
      val (generic, concrete) = decls.partition(_._1.generic)

      // How many overloads of each name a generic clause offers, vs. what each concrete element type offers.
      val byGenericName = generic.groupBy(d => (d._1.wrapper, d._2))
      val byConcreteName = concrete.groupBy(d => (d._1.wrapper, d._1.elem, d._2))
      def where(ds: Seq[(Clause, String)]) = ds.map(_._1.where).distinct.sorted.mkString(", ")

      byConcreteName.toSeq.sortBy(_._1).foreach { case ((wrapper, elem, name), narrowDecls) =>
        byGenericName.get((wrapper, name)).foreach { wideDecls =>
          val line = s"  [$platform] $wrapper[$elem].$name: ${narrowDecls.size} overload(s) in " +
            s"${where(narrowDecls)} vs ${wideDecls.size} in the generic $wrapper[A] clause(s) at ${where(wideDecls)}"
          if narrowDecls.size < wideDecls.size then problems += line else notes += line
          end if
        }
      }
    }

    if notes.nonEmpty then
      println(s"[shape coverage] names redeclared at both specificity levels, fully covered:\n${notes.mkString("\n")}")
    end if

    assert(
      problems.isEmpty,
      "A concretely-typed extension clause redeclares a method name without covering every shape the generic\n" +
        "clause offers. Scala resolves the call against the narrower clause and will not fall back, so\n" +
        "already-working call sites using an uncovered shape stop compiling (see the class comment, and\n" +
        s"github.com/Quafadas/vecxt/issues/105):\n${problems.mkString("\n")}"
    )
  }

end ExtensionShadowingSuite

/** Fixtures for the `resolution:` probes above. Kept out of the suite so the probe strings can name them by a stable
  * path. `get(l: Long)` in the narrow clauses is not useful in itself - it exists so that the name `get` appears at two
  * receiver-specificity levels without any two definitions sharing an erased signature.
  */
object shadowProbe:

  final case class Box[A](x: A)

  object GenericClause:
    extension [A](b: Box[A])
      def get(i: Int): A = b.x
      def get(is: Array[Int]): A = b.x
    end extension
  end GenericClause

  object NarrowClause:
    extension (b: Box[Double]) def get(l: Long): Double = b.x
    end extension
  end NarrowClause

  /** Both clauses in one object - the shape the reverted `ndarrayOps` attempt had. */
  object OneObject:
    extension [A](b: Box[A])
      def get(i: Int): A = b.x
      def get(is: Array[Int]): A = b.x
    end extension

    extension (b: Box[Double]) def get(l: Long): Double = b.x
    end extension
  end OneObject

  /** Separate objects funnelled through one aggregator, as `vecxt.all` does. */
  object Reexported:
    export GenericClause.*
    export NarrowClause.*
  end Reexported

end shadowProbe

/** A high-fidelity replica of what the reverted `ndarrayOps` attempt actually looked like, because the simplified
  * `shadowProbe` fixtures say a same-object narrow clause does *not* hide the generic clause's other shapes - which
  * contradicts the reported breakage. Everything the real attempt had and `shadowProbe` lacks is reproduced here: a
  * `final class` wrapper, `inline` members, a vararg of a union type, an `Array[Int]` shape that the union also admits,
  * a `using ClassTag` on the generic vararg arm, and five narrow clauses discriminated by `@targetName`.
  *
  * If `nd(0)` resolves here, the reverted attempt failed for some other reason and the cheap one-overload-per-type fix
  * for `NDArray[A]#apply(selectors*)` is viable after all.
  */
object ndarrayReplica:

  type Selector = Range | Array[Int]

  final class Nd[A](val data: Array[A])

  object Ops:
    // Bodies are `???` deliberately: a real `arr.data(i0)` here is a generic array access, so it would emit the very
    // ScalaRunTime symbols the sibling C6a suite scans for and show up as a fresh finding in its report. Only the
    // signatures matter for `typeChecks`.
    extension [A](arr: Nd[A])
      inline def apply(i0: Int): A = ???
      inline def apply(i0: Int, i1: Int): A = ???
      inline def apply(i0: Int, i1: Int, i2: Int): A = ???
      inline def apply(indices: Array[Int]): A = ???
      def apply(selectors: Selector*)(using ClassTag[A]): Nd[A] = arr
    end extension

    extension (arr: Nd[Double])
      @targetName("applyDoubleSelectors")
      def apply(selectors: Selector*): Nd[Double] = arr
    end extension

    extension (arr: Nd[Float])
      @targetName("applyFloatSelectors")
      def apply(selectors: Selector*): Nd[Float] = arr
    end extension

    extension (arr: Nd[Int])
      @targetName("applyIntSelectors")
      def apply(selectors: Selector*): Nd[Int] = arr
    end extension

    extension (arr: Nd[Long])
      @targetName("applyLongSelectors")
      def apply(selectors: Selector*): Nd[Long] = arr
    end extension

    extension (arr: Nd[Boolean])
      @targetName("applyBooleanSelectors")
      def apply(selectors: Selector*): Nd[Boolean] = arr
    end extension
  end Ops

end ndarrayReplica
