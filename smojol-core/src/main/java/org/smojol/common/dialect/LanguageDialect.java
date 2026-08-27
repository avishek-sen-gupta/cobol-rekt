package org.smojol.common.dialect;

import java.util.List;
import java.util.logging.Logger;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.AnalysisConfig;
import org.eclipse.lsp.cobol.common.copybook.CopybookProcessingMode;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;

public enum LanguageDialect {
  COBOL {
    @Override
    public AnalysisConfig analysisConfig(String dialectConfigJarPath) {
      return AnalysisConfig.substitutingDefaultConfig(CopybookProcessingMode.ENABLED);
    }

    @Override
    public void verifyNoNullDialectStatements(
        ParserRuleContext tree, EntityNavigatorBuilder navigatorBuilder) {}
  },
  IDMS {
    @Override
    public AnalysisConfig analysisConfig(String dialectConfigJarPath) {
      return AnalysisConfig.idmsConfig(dialectConfigJarPath, CopybookProcessingMode.ENABLED);
    }

    @Override
    public void verifyNoNullDialectStatements(
        ParserRuleContext tree, EntityNavigatorBuilder navigatorBuilder) {
      CobolEntityNavigator navigator = navigatorBuilder.navigator(tree);
      List<ParseTree> nullDialectStatements =
          navigator.findAllByCondition(
              n ->
                  n.getClass() == CobolParser.DialectStatementContext.class
                      && ((CobolParser.DialectStatementContext) n).dialectNodeFiller() != null
                      && !coveredByFragment(
                          ((CobolParser.DialectStatementContext) n).dialectNodeFiller()),
              tree);
      nullDialectStatements.forEach(
          n -> {
            boolean removed = ((ParserRuleContext) n.getParent()).children.remove(n);
            java.util.logging.Logger logger = Logger.getLogger(LanguageDialect.class.getName());
            logger.finer(removed ? "removed" : "not removed");
          });
      List<ParseTree> nullIdmsNodes =
          navigator.findAllByCondition(
              n ->
                  n.getClass() == CobolParser.DialectNodeFillerContext.class
                      && !coveredByFragment((ParserRuleContext) n),
              tree);

      if (!nullIdmsNodes.isEmpty())
        throw new RuntimeException("Null IDMS nodes detected, please run preprocess()");
    }
  };

  public static LanguageDialect dialect(String dialectAsString) {
    if (dialectAsString == null) return COBOL;
    else if (dialectAsString.equals(IDMS.name())) return IDMS;
    else if (dialectAsString.equals(COBOL.name())) return COBOL;
    else return COBOL;
  }

  /**
   * Whether a filler context sits inside a region that a dialect visitor recorded as a fragment.
   * Replaces the old test for the absence of a {@code _DIALECT_ <guid>} marker.
   */
  private static boolean coveredByFragment(ParserRuleContext ctx) {
    Token start = ctx.getStart();
    return start != null
        && PersistentData.isCovered(start.getLine(), start.getCharPositionInLine());
  }

  public abstract AnalysisConfig analysisConfig(String dialectConfigJarPath);

  public abstract void verifyNoNullDialectStatements(
      ParserRuleContext tree, EntityNavigatorBuilder navigatorBuilder);
}
