package org.smojol.common.idms;

import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link DialectIntegratorListener} handling of missing keys in
 * {@link PersistentData}.
 *
 * <p>Issue 1 (HIGH): When the IDMS key requested by a {@code dialectNodeFiller} placeholder
 * is absent from {@code PersistentData}, the listener must skip gracefully rather than
 * throwing a {@code NullPointerException}.
 */
class DialectIntegratorListenerMissingKeyTest {

    @BeforeEach
    void resetPersistentData() {
        PersistentData.reset();
    }

    /**
     * When {@code PersistentData} has no entry for a given IDMS key, calling
     * {@code enterDialectNodeFiller} must not throw any exception.
     *
     * <p>Before the fix, {@code PersistentData.dialect("IDMS-99")} called
     * {@code Objects.requireNonNull(getDialectNode(...))} which would throw a bare
     * {@code NullPointerException} with no diagnostic message.
     */
    @Test
    void missingKeyDoesNotThrowNullPointerException() {
        // PersistentData is empty (reset in @BeforeEach) — no "IDMS-99" entry exists
        DialectIntegratorListener listener = new DialectIntegratorListener();

        CobolParser.DialectGuidContext guidCtx = Mockito.mock(CobolParser.DialectGuidContext.class);
        Mockito.when(guidCtx.getText()).thenReturn("99");

        CobolParser.DialectNodeFillerContext fillerCtx =
                Mockito.mock(CobolParser.DialectNodeFillerContext.class);
        Mockito.when(fillerCtx.dialectGuid()).thenReturn(guidCtx);

        assertDoesNotThrow(() -> listener.enterDialectNodeFiller(fillerCtx),
                "enterDialectNodeFiller must not throw when the IDMS key is absent from PersistentData");
    }

    /**
     * When the key is missing, no node should be reinjected — {@code restores} must remain 0.
     */
    @Test
    void missingKeyProducesZeroRestores() {
        DialectIntegratorListener listener = new DialectIntegratorListener();

        CobolParser.DialectGuidContext guidCtx = Mockito.mock(CobolParser.DialectGuidContext.class);
        Mockito.when(guidCtx.getText()).thenReturn("99");

        CobolParser.DialectNodeFillerContext fillerCtx =
                Mockito.mock(CobolParser.DialectNodeFillerContext.class);
        Mockito.when(fillerCtx.dialectGuid()).thenReturn(guidCtx);

        listener.enterDialectNodeFiller(fillerCtx);

        assertEquals(0, listener.getRestores(),
                "No restore must be counted when the IDMS key is absent from PersistentData");
    }

    /**
     * A null {@code dialectGuid()} (guard already present) must still be handled without
     * exception, confirming the early-return path for null guid is unaffected by the new fix.
     */
    @Test
    void nullDialectGuidIsHandledGracefully() {
        DialectIntegratorListener listener = new DialectIntegratorListener();

        CobolParser.DialectNodeFillerContext fillerCtx =
                Mockito.mock(CobolParser.DialectNodeFillerContext.class);
        Mockito.when(fillerCtx.dialectGuid()).thenReturn(null);

        assertDoesNotThrow(() -> listener.enterDialectNodeFiller(fillerCtx),
                "A null dialectGuid must be handled without exception");
        assertEquals(0, listener.getRestores(), "No restore must be counted for null guid");
    }
}
