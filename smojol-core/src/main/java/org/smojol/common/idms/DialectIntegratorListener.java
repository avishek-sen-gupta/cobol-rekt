package org.smojol.common.idms;

import java.util.List;
import java.util.logging.Logger;
import lombok.Getter;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.CobolParserBaseListener;

/**
 * This is a visitor into the generated parse tree which re-integrates the dialect code fragments
 * which were removed at the time of parsing standard Cobol.
 *
 * <p>Correlation is positional: every dialect visitor blanks its fragment out of the extended
 * document length-preservingly, so a fragment's recorded start position is still the position of
 * the filler run the COBOL parser produced in its place.
 *
 * <p>Claiming is per <em>filler token</em>, not per filler context, because a context does not
 * correspond one-to-one with a fragment. The grammar rule is {@code ZERO_WIDTH_SPACE+ ...}, and the
 * lexer emits one {@code ZERO_WIDTH_SPACE} token per blanked source line, so:
 *
 * <ul>
 *   <li>a single fragment blanked across N lines yields N tokens in one context, and must still be
 *       grafted exactly once; and
 *   <li>two <em>adjacent</em> fragments with no intervening {@code DOT_FS} are swallowed by the
 *       greedy {@code +} into one context, and each must still get its own graft.
 * </ul>
 *
 * <p>{@code claimedThroughLine} is what separates those two cases: after a successful claim, every
 * token up to and including the claimed fragment's {@code endLine} belongs to that same fragment
 * and is skipped. The first token past it starts a new fragment.
 *
 * <p>Known limitation: two adjacent fragments on the <em>same</em> line still collapse into a
 * single graft. Telling them apart needs the end column, which {@code PersistentData.Fragment} does
 * not record.
 */
public class DialectIntegratorListener extends CobolParserBaseListener {
  private static final Logger LOGGER = Logger.getLogger(DialectIntegratorListener.class.getName());
  @Getter private int restores = 0;

  @Override
  public void enterDialectNodeFiller(CobolParser.DialectNodeFillerContext ctx) {
    super.enterDialectNodeFiller(ctx);
    List<TerminalNode> fillers = ctx.ZERO_WIDTH_SPACE();
    if (fillers == null) return;
    int claimedThroughLine = -1;
    for (TerminalNode filler : fillers) {
      if (filler == null) continue;
      Token token = filler.getSymbol();
      if (token == null) continue;
      int line = token.getLine();
      int charPos = token.getCharPositionInLine();
      // Still inside the fragment claimed by an earlier token of this same run.
      if (line <= claimedThroughLine) continue;
      PersistentData.Fragment fragment = PersistentData.claim(line, charPos);
      if (fragment == null) {
        LOGGER.finer(
            String.format(
                "No unclaimed dialect fragment covers %d:%d; skipping reinjection", line, charPos));
        continue;
      }
      LOGGER.finer(
          String.format(
              "Restoring %s fragment recorded at %d:%d: %s",
              fragment.dialect, fragment.startLine, fragment.startChar, fragment.tree.getText()));
      ctx.addChild(new DialectContainerNode(fragment.tree, ctx, fragment.dialect));
      restores++;
      claimedThroughLine = fragment.endLine;
    }
  }
}
