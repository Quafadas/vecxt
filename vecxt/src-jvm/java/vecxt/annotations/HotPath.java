package vecxt.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An inner-loop kernel: code that runs once per element, and whose cost therefore scales with the
 * data rather than with the number of calls.
 *
 * <p>Declared in Java rather than Scala on purpose. Scala 3 dropped {@code ClassfileAnnotation}, so
 * an annotation defined in Scala lives in TASTy only and is invisible to bytecode analysis; a
 * Java-declared annotation with {@code RUNTIME} retention lands in {@code RuntimeVisibleAnnotations}
 * where the audit can read it. {@code vecxt/src-js-native/annotations.scala} declares no-op Scala
 * equivalents under the same names so cross-platform sources can carry the annotation too.
 *
 * <p>What the audit asserts about a {@code @HotPath} method (check C2 of
 * <a href="https://github.com/Quafadas/vecxt/issues/105">#105</a>): its emitted bytecode fits inside
 * HotSpot's {@code FreqInlineSize}. A kernel larger than that budget is not inlined however hot it
 * gets, which costs the surrounding loop the optimisations that only happen across an inlined
 * boundary.
 *
 * <p><b>That is a necessary condition, not a sufficient one.</b> An earlier version of this note
 * claimed the check meant "C2 will inline it into its callers once it is hot". It does not.
 * {@code InlineSmallCode} (2500) applies to an already-compiled callee and is measured in
 * <i>machine code</i>, not bytecode — and for vectorised kernels the observed expansion is 7–10×,
 * so 2500 bytes of machine code is reached at roughly 260–300 bytecodes, below
 * {@code FreqInlineSize}'s 325. A kernel can satisfy this annotation and still be one C2 declines
 * to inline.
 *
 * <p>Bytecode analysis cannot see that, so C2 asserts what it can. The compiled size is check D2's
 * business: it reads {@code stub_offset - insts_offset} off {@code LogCompilation}'s {@code c2}
 * {@code <nmethod>} elements. Until D2 lands, a kernel in the upper part of the
 * {@code FreqInlineSize} range should be treated as unverified rather than safe. See
 * {@code site/docs/blog/2026-07-28-Inlining.md}, "The limit that is not measured in bytecodes",
 * for the measurements.
 *
 * <p><b>Only meaningful on a method that is actually emitted.</b> An {@code inline def} body is
 * expanded into its callers instead of being compiled on its own, so it has no bytecode to measure
 * and annotating one is a mistake the audit reports (check A1) rather than silently ignores.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
public @interface HotPath {}
