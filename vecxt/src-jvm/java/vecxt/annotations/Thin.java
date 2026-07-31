package vecxt.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An API forwarder: a method that exists to give an operation a name, and whose body is a call to
 * the method that does the work.
 *
 * <p>The audit asserts (check C3 of
 * <a href="https://github.com/Quafadas/vecxt/issues/105">#105</a>) that it fits inside HotSpot's
 * {@code MaxInlineSize}, the budget below which C2 inlines a callee <i>regardless of how often it is
 * called</i>. That is the property that makes the public API zero-cost at a cold or lukewarm call
 * site, where {@code FreqInlineSize} does not apply yet.
 *
 * <p>It also asserts the method contains no backward branch. A loop in a forwarder means the method
 * does per-element work, so the annotation is simply the wrong one — {@link HotPath} is.
 *
 * <p>Like {@link HotPath}, this says nothing about an {@code inline def}: there is no emitted body to
 * measure. See {@link HotPath} for why these are Java declarations.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
public @interface Thin {}
