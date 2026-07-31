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
 * HotSpot's {@code FreqInlineSize}, so C2 will inline it into its callers once it is hot. A kernel
 * larger than that budget is not inlined however hot it gets, which costs the surrounding loop the
 * optimisations that only happen across an inlined boundary.
 *
 * <p><b>Only meaningful on a method that is actually emitted.</b> An {@code inline def} body is
 * expanded into its callers instead of being compiled on its own, so it has no bytecode to measure
 * and annotating one is a mistake the audit reports (check A1) rather than silently ignores.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
public @interface HotPath {}
