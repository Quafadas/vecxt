package vecxt.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A kernel that must allocate nothing per operation once warm.
 *
 * <p>Unlike {@link HotPath} and {@link Thin}, this one carries no static check: whether a
 * {@code Vector} object survives as a real allocation or is scalarized away by escape analysis is a
 * property of the compiled code, not of the bytecode. It is asserted dynamically in Phase 2 of
 * <a href="https://github.com/Quafadas/vecxt/issues/105">#105</a> (check D1) by measuring
 * {@code ThreadMXBean.getThreadAllocatedBytes} across a warmed loop and requiring bytes-per-op to be
 * zero.
 *
 * <p>Until then the annotation is a declaration of intent, and the audit reports how many methods
 * carry it so the Phase 2 harness has an explicit work list rather than a guess. It is deliberately
 * applied only where the body allocates nothing textually — a method returning a fresh array is not
 * a candidate however hot it is.
 *
 * <p>See {@link HotPath} for why these are Java declarations, and for why annotating an
 * {@code inline def} is a mistake.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
public @interface AllocFree {}
