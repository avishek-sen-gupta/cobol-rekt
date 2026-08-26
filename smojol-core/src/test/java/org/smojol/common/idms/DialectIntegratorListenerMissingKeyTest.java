package org.smojol.common.idms;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.ParserRuleContext;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mockito;

/**
 * Unit tests for {@link DialectIntegratorListener} correlation against {@link PersistentData}.
 *
 * <p>When no recorded fragment covers a {@code dialectNodeFiller}'s position, the listener must
 * skip gracefully rather than throw. That happens legitimately: upstream blanks some regions
 * (for example IDMS {@code visitObtainLRStatement}) without the fork recording a fragment, so
 * unclaimable fillers are expected, not exceptional.
 */
@Execution(ExecutionMode.SAME_THREAD)
class DialectIntegratorListenerMissingKeyTest {

  @BeforeEach
  void resetPersistentData() {
    PersistentData.reset();
  }

  private static CobolParser.DialectNodeFillerContext fillerAt(int line, int charPos) {
    CommonToken start = new CommonToken(1, "​");
    start.setLine(line);
    start.setCharPositionInLine(charPos);
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.getStart()).thenReturn(start);
    return ctx;
  }

  private static ParserRuleContext dialectTreeAt(int line, int charPos) {
    ParserRuleContext ctx = new ParserRuleContext();
    CommonToken token = new CommonToken(1, "FINISH");
    token.setLine(line);
    token.setCharPositionInLine(charPos);
    ctx.start = token;
    ctx.stop = token;
    return ctx;
  }

  @Test
  void uncoveredPositionDoesNotThrow() {
    DialectIntegratorListener listener = new DialectIntegratorListener();

    assertDoesNotThrow(
        () -> listener.enterDialectNodeFiller(fillerAt(99, 0)),
        "enterDialectNodeFiller must not throw when no fragment covers the position");
  }

  @Test
  void uncoveredPositionProducesZeroRestores() {
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerAt(99, 0));

    assertEquals(
        0, listener.getRestores(), "No restore must be counted for an uncovered filler position");
  }

  @Test
  void nullStartTokenIsHandledGracefully() {
    DialectIntegratorListener listener = new DialectIntegratorListener();
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.getStart()).thenReturn(null);

    assertDoesNotThrow(
        () -> listener.enterDialectNodeFiller(ctx), "A null start token must not throw");
    assertEquals(0, listener.getRestores(), "No restore must be counted for a null start token");
  }

  @Test
  void coveredPositionGraftsTheFragmentAndCountsOneRestore() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerAt(12, 11));

    assertEquals(1, listener.getRestores(), "A covered filler position must graft exactly once");
  }

  @Test
  void aFragmentIsGraftedAtMostOnceAcrossTwoFillers() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerAt(12, 11));
    listener.enterDialectNodeFiller(fillerAt(12, 11));

    assertEquals(
        1,
        listener.getRestores(),
        "claim() consumes the fragment, so a second filler at the same position must not graft");
  }

  @Test
  void positionRightOfStartOnTheStartLineStillGrafts() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    // IDMS prepends "_IF_ " (5 chars), shifting the filler run right of the recorded start.
    listener.enterDialectNodeFiller(fillerAt(12, 16));

    assertEquals(1, listener.getRestores(), "The _IF_ prefix must not break correlation");
  }
}
