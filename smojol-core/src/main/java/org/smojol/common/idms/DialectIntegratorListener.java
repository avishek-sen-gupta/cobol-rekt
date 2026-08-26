package org.smojol.common.idms;

import java.util.logging.Logger;
import lombok.Getter;
import org.antlr.v4.runtime.Token;
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
 */
public class DialectIntegratorListener extends CobolParserBaseListener {
  private static final Logger LOGGER = Logger.getLogger(DialectIntegratorListener.class.getName());
  @Getter private int restores = 0;

  @Override
  public void enterDialectNodeFiller(CobolParser.DialectNodeFillerContext ctx) {
    super.enterDialectNodeFiller(ctx);
    Token start = ctx.getStart();
    if (start == null) return;
    PersistentData.Fragment fragment =
        PersistentData.claim(start.getLine(), start.getCharPositionInLine());
    if (fragment == null) {
      LOGGER.finer(
          String.format(
              "No unclaimed dialect fragment covers %d:%d; skipping reinjection",
              start.getLine(), start.getCharPositionInLine()));
      return;
    }
    LOGGER.finer(
        String.format(
            "Restoring %s fragment recorded at %d:%d: %s",
            fragment.dialect, fragment.startLine, fragment.startChar, fragment.tree.getText()));
    ctx.addChild(new DialectContainerNode(fragment.tree, ctx, fragment.dialect));
    restores++;
  }
}
