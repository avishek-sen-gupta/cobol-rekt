package org.smojol.common.ast;

import java.util.function.Function;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.CobolEntityNavigator;

public class NodeText {
  public static String PASSTHROUGH(String text) {
    return text;
  }

  public static String originalText(ParseTree astNode) {
    return originalText(astNode, NodeText::PASSTHROUGH);
  }

  public static String dialectOriginalText(ParseTree astNode, FlowNodeService nodeService) {
    CobolEntityNavigator navigator = nodeService.getNavigator();
    ParseTree filler =
        navigator.findByCondition(
            astNode, t -> t.getClass() == CobolParser.DialectNodeFillerContext.class);
    if (filler == null) return astNode.getText();
    Token start = ((CobolParser.DialectNodeFillerContext) filler).getStart();
    if (start == null) return astNode.getText();
    PersistentData.Fragment fragment =
        PersistentData.fragmentAt(start.getLine(), start.getCharPositionInLine());
    if (fragment == null) return astNode.getText();
    return NodeText.originalText(fragment.tree, NodeText::PASSTHROUGH);
  }

  public static String originalText(
      ParseTree astNode, Function<String, String> substitutionStrategy) {
    Token startToken =
        (astNode instanceof TerminalNode)
            ? ((TerminalNode) astNode).getSymbol()
            : ((ParserRuleContext) astNode).start;
    Token stopToken =
        (astNode instanceof TerminalNode)
            ? ((TerminalNode) astNode).getSymbol()
            : ((ParserRuleContext) astNode).stop;

    if (startToken == null) return astNode.getText();
    CharStream cs = startToken.getInputStream();
    int stopIndex = stopToken != null ? stopToken.getStopIndex() : -1;
    if (cs == null) {
      return astNode.getText();
    }
    Interval interval = new Interval(startToken.getStartIndex(), stopIndex);
    if (interval.a == -1 || interval.b == -1) {
      return astNode.getText();
    }
    return stopIndex >= startToken.getStartIndex()
        ? dialectInlined(cs.getText(interval), substitutionStrategy)
        : "<NULL>";
  }

  /**
   * Returns the source text unchanged.
   *
   * <p>This used to rewrite each {@code _DIALECT_ <guid>} marker that the che4z fork injected in
   * place of a dialect fragment, handing the marker to {@code substitutionStrategy} so a caller
   * could splice the original dialect text back in. The fork no longer emits any marker: dialect
   * fragments are blanked out length-preservingly and correlated to the COBOL parse tree by
   * document position, so no marker can appear in {@code text} and there is nothing to substitute.
   *
   * <p>{@code substitutionStrategy} is consequently inert for every caller of {@link
   * #originalText(ParseTree, Function)}. Retiring that parameter, and the marker-keyed repository
   * in {@code CobolEntityNavigator} that feeds it, is a follow-up cleanup.
   */
  private static String dialectInlined(String text, Function<String, String> substitutionStrategy) {
    return text;
  }

  public static String formatted(String s) {
    return s.replace("\n", " ").replace("\t", " ").trim();
  }
}
