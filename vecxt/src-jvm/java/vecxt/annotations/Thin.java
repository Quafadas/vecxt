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
 * <p><b>The budget is in bytecodes and says nothing about the compiled form.</b> Measured on a
 * {@code LogCompilation} run: {@code vecxt.all.clamp!} is an eleven-bytecode {@code export}
 * forwarder whose {@code c2} nmethod is 1696 bytes of machine code, because C2 inlined the kernel
 * into it — 68% of {@code InlineSmallCode}. It would satisfy this annotation's 35-byte budget by a
 * factor of three while being one of the largest compiled methods in the library.
 *
 * <p>Two reasons that matters more here than for {@link HotPath}. Forwarders are where the
 * bytecode-to-machine-code ratio is most extreme, precisely because the body they forward to gets
 * pulled in. And the {@code vecxt.all} export forwarders are excluded from the checked-in baseline
 * by {@code Audit.primaryAnnotated} — deliberately, to keep the baseline readable — so their
 * compiled size is unmeasured twice over.
 *
 * <p>Unverified, and worth confirming in the HotSpot source before relying on either answer: whether
 * the {@code MaxTrivialSize}/{@code MaxInlineSize} fast paths let a small callee bypass the
 * {@code InlineSmallCode} veto. If they do, the forwarder above is a curiosity. If they do not, a
 * small forwarder with a large nmethod stops being inlinable, and this annotation is asserting the
 * wrong quantity for exactly the methods it was written for.
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
