package vecxt.annotations

/** No-op JS/Native counterparts of the JVM audit annotations.
  *
  * The annotations are Java declarations under `vecxt/src-jvm/java/vecxt/annotations/` so that they reach
  * `RuntimeVisibleAnnotations` and the bytecode audit can read them — Scala 3 dropped `ClassfileAnnotation`, so a
  * Scala-declared annotation would live in TASTy only. That leaves JS and Native with no such class, which would make
  * any annotated source under `vecxt/src/` fail to compile on those platforms.
  *
  * Declaring no-op equivalents here rather than confining every annotated method to `src-jvm` is the maintainer's
  * choice, recorded in https://github.com/Quafadas/vecxt/issues/105: the alternative forces a kernel to move platforms
  * for the sake of the audit, which is the audit dictating the source layout rather than describing it.
  *
  * They carry no meaning on these platforms and no check reads them. Neither Scala.js nor Scala Native has HotSpot's
  * inlining budgets — the equivalent questions there are the linker's and LLVM's, and are Phase 3's (Tier 3) business.
  */

final class HotPath extends scala.annotation.StaticAnnotation

final class Thin extends scala.annotation.StaticAnnotation

final class AllocFree extends scala.annotation.StaticAnnotation
