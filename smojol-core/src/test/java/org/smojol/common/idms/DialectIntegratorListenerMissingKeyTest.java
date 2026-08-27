package org.smojol.common.idms;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.tree.TerminalNodeImpl;
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
 * skip gracefully rather than throw. That happens legitimately: upstream blanks some regions (for
 * example IDMS {@code visitObtainLRStatement}) without the fork recording a fragment, so
 * unclaimable fillers are expected, not exceptional.
 */
@Execution(ExecutionMode.SAME_THREAD)
class DialectIntegratorListenerMissingKeyTest {

  @BeforeEach
  void resetPersistentData() {
    PersistentData.reset();
  }

  /** A filler context holding exactly one ZERO_WIDTH_SPACE token, i.e. a single blanked line. */
  private static CobolParser.DialectNodeFillerContext fillerAt(int line, int charPos) {
    return fillerRun(new int[][] {{line, charPos}});
  }

  /**
   * A filler context holding one ZERO_WIDTH_SPACE token per {@code {line, charPos}} pair, which is
   * what the lexer produces for a blanked region spanning several source lines — whether those
   * lines belong to one fragment or to several adjacent ones.
   */
  private static CobolParser.DialectNodeFillerContext fillerRun(int[][] positions) {
    List<TerminalNode> fillers =
        Arrays.stream(positions)
            .map(p -> fillerToken(p[0], p[1]))
            .map(TerminalNode.class::cast)
            .toList();
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.ZERO_WIDTH_SPACE()).thenReturn(fillers);
    Mockito.when(ctx.getStart()).thenReturn(fillers.get(0).getSymbol());
    return ctx;
  }

  private static TerminalNodeImpl fillerToken(int line, int charPos) {
    CommonToken token = new CommonToken(CobolParser.ZERO_WIDTH_SPACE, "​");
    token.setLine(line);
    token.setCharPositionInLine(charPos);
    return new TerminalNodeImpl(token);
  }

  private static ParserRuleContext dialectTreeAt(int line, int charPos) {
    return dialectTreeSpanning(line, charPos, line);
  }

  /** A stand-in dialect parse tree whose start/stop tokens span {@code startLine..endLine}. */
  private static ParserRuleContext dialectTreeSpanning(int startLine, int charPos, int endLine) {
    ParserRuleContext ctx = new ParserRuleContext();
    CommonToken start = new CommonToken(1, "FINISH");
    start.setLine(startLine);
    start.setCharPositionInLine(charPos);
    CommonToken stop = new CommonToken(1, "TASK");
    stop.setLine(endLine);
    stop.setCharPositionInLine(charPos);
    ctx.start = start;
    ctx.stop = stop;
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
  void anEmptyFillerRunIsHandledGracefully() {
    DialectIntegratorListener listener = new DialectIntegratorListener();
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.ZERO_WIDTH_SPACE()).thenReturn(List.of());

    assertDoesNotThrow(
        () -> listener.enterDialectNodeFiller(ctx),
        "A filler context with no tokens must not throw");
    assertEquals(0, listener.getRestores(), "No restore must be counted for an empty filler run");
  }

  @Test
  void aNullFillerAccessorResultIsHandledGracefully() {
    DialectIntegratorListener listener = new DialectIntegratorListener();
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.ZERO_WIDTH_SPACE()).thenReturn(null);

    assertDoesNotThrow(
        () -> listener.enterDialectNodeFiller(ctx),
        "A null ZERO_WIDTH_SPACE() result must not throw");
    assertEquals(0, listener.getRestores(), "No restore must be counted for a null filler list");
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

  /**
   * Two adjacent fragments with no intervening {@code DOT_FS} are swallowed by the greedy {@code +}
   * in {@code dialectNodeFiller : ZERO_WIDTH_SPACE+ ...} into a single filler context. Claiming
   * once per context grafted only the first and silently dropped the second, so claiming must be
   * per filler token instead.
   */
  @Test
  void twoAdjacentFragmentsInOneContextAreBothGrafted() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    PersistentData.record(dialectTreeAt(13, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerRun(new int[][] {{12, 11}, {13, 11}}));

    assertEquals(
        2,
        listener.getRestores(),
        "Both adjacent fragments collapsed into one filler context must be grafted");
  }

  /**
   * The counterpart invariant: a single fragment blanked across several lines also produces several
   * filler tokens in one context, and must still graft exactly once.
   *
   * <p>Note this one holds for a weaker reason than it looks: {@code claim} consumes, so the second
   * and third tokens find the fragment already claimed and graft nothing even without {@code
   * claimedThroughLine}. What the guard is actually load-bearing for is {@link
   * #aNestedFragmentInsideAlreadyClaimedLinesIsNotGraftedSeparately}.
   */
  @Test
  void aSingleMultiLineFragmentIsGraftedExactlyOnce() {
    PersistentData.record(dialectTreeSpanning(12, 11, 14), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerRun(new int[][] {{12, 11}, {13, 11}, {14, 11}}));

    assertEquals(
        1,
        listener.getRestores(),
        "A single fragment spanning lines 12-14 must be grafted once, not once per blanked line");
  }

  /**
   * Recorded fragments nest: {@code IdmsSubstitutingVisitor} records {@code idmsIfStatement} and
   * then the {@code idmsIfCondition} inside it, so an inner fragment's tree is already a subtree of
   * an outer fragment's tree. Grafting the inner one separately would attach the same dialect
   * subtree to the COBOL tree twice.
   *
   * <p>Consuming claims alone do not prevent that — the inner fragment is a <em>different</em>,
   * still-unclaimed fragment, so a token on its line claims it happily. {@code claimedThroughLine}
   * is the only thing that suppresses it: after the outer fragment is claimed, every token up to and
   * including its {@code endLine} is known to belong to it. Deleting the guard turns this test red.
   */
  @Test
  void aNestedFragmentInsideAlreadyClaimedLinesIsNotGraftedSeparately() {
    PersistentData.record(dialectTreeSpanning(12, 11, 14), LocalisedDialect.IDMS);
    PersistentData.record(dialectTreeAt(13, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerRun(new int[][] {{12, 11}, {13, 11}, {14, 11}}));

    assertEquals(
        1,
        listener.getRestores(),
        "Only the outer fragment may be grafted; the nested one is already inside its subtree");
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
