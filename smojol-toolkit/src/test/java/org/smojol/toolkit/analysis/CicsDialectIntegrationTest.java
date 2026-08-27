package org.smojol.toolkit.analysis;

import static org.junit.jupiter.api.Assertions.*;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.UUIDProvider;
import java.io.File;
import java.io.IOException;
import java.util.List;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.idms.DialectContainerNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.analysis.validation.DataStructureValidation;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

/**
 * End-to-end coverage for the CICS half of the grafting mechanism, which had none.
 *
 * <p>The CICS substituting visitor is not optional capability behind an unset flag: {@code
 * LanguageDialect.COBOL} and {@code LanguageDialect.IDMS} both build their {@code AnalysisConfig}
 * from {@code AnalysisConfig.substitutingDefaultConfig} / {@code AnalysisConfig.idmsConfig}, and
 * both of those enable the {@code addCicsPlaceholder} field rather than leaving it off. So every
 * smojol parse — of any COBOL file, with or without a dialect — runs the substituting CICS visitor.
 * Until this test, nothing in the parent repo exercised that path; the only coverage was the
 * unit-level equivalence test inside the submodule, which drives {@code CICSDialect} directly
 * rather than through {@code ParsePipeline}.
 *
 * <p>Mechanically this is the same two-phase, marker-free, positionally-correlated scheme that
 * {@link IdmsDialectIntegrationTest} documents for IDMS: {@code CicsSubstitutingVisitor} blanks
 * each {@code EXEC CICS} block out of the extended document length-preservingly and records the
 * region in {@link PersistentData} along with the CICS parse tree; {@code
 * DialectIntegratorListener} then claims each recorded fragment at the position of the filler run
 * the COBOL parser matched in its place and attaches a {@code DialectContainerNode}.
 *
 * <p>Unlike IDMS, CICS needs no dialect jar — it is an <em>implicit</em> dialect, processed by
 * {@code ImplicitDialectProcessingStage} from inside the engine, which is why the fixture parses
 * under {@code LanguageDialect.COBOL} with a {@code "NONE"} dialect jar path.
 */
@Execution(ExecutionMode.SAME_THREAD)
public class CicsDialectIntegrationTest {

  /**
   * cics.cbl has 3 EXEC CICS blocks: {@code WRITEQ TD} (multi-line), {@code WRITE OPERATOR}
   * (multi-line) and {@code RETURN} (single-line, and the only one that shares its line with {@code
   * END-EXEC}).
   */
  private static final int EXEC_CICS_BLOCKS_IN_FIXTURE = 3;

  @BeforeEach
  void resetPersistentData() {
    PersistentData.reset();
  }

  @Test
  void canParseCicsCobolThroughTheRealPipeline() throws IOException {
    CobolEntityNavigator navigator = cicsPipeline().parse(DataStructureValidation.NO_BUILD);

    assertNotNull(navigator, "Navigator must be returned after parsing a CICS-containing program");
  }

  /**
   * The core assertion: every EXEC CICS block the substituting visitor blanked out must come back
   * as a grafted {@code DialectContainerNode}. A lower count means a fragment was recorded, removed
   * from the document, and then silently lost its graft anchor — which is exactly the failure mode
   * that adjacent-fragment collapse produced before {@code DialectIntegratorListener} started
   * claiming once per filler token rather than once per filler context.
   */
  @Test
  void everyExecCicsBlockIsGraftedBackAsADialectContainerNode() throws IOException {
    CobolEntityNavigator navigator = cicsPipeline().parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(
        EXEC_CICS_BLOCKS_IN_FIXTURE,
        dialectNodes.size(),
        "Expected one DialectContainerNode per EXEC CICS block in cics.cbl");
  }

  @Test
  void everyGraftedNodeCarriesTheCicsDialect() throws IOException {
    CobolEntityNavigator navigator = cicsPipeline().parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      assertEquals(
          LocalisedDialect.CICS,
          dcn.getDialect(),
          "Every node grafted from a CICS fragment must carry LocalisedDialect.CICS; got "
              + dcn.getDialect()
              + " for: "
              + dcn.getText());
    }
  }

  /**
   * Every recorded fragment must be accounted for by a graft. This is the parse-level form of the
   * reconciliation {@code ParsePipeline} now logs at WARNING, asserted rather than merely logged.
   */
  @Test
  void everyRecordedCicsFragmentIsClaimed() throws IOException {
    CobolEntityNavigator navigator = cicsPipeline().parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(
        PersistentData.fragmentCount(),
        dialectNodes.size(),
        "Every fragment the CICS visitor recorded must be grafted back; a shortfall means a"
            + " blanked EXEC CICS block vanished from the parse tree");
  }

  /**
   * The grafted text must be the original EXEC CICS source, reconstructed from the retained CICS
   * parse tree rather than read back out of the blanked document.
   */
  @Test
  void graftedNodesReturnTheOriginalExecCicsText() throws IOException {
    CobolEntityNavigator navigator = cicsPipeline().parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      String text = node.getText();
      assertNotNull(text, "DialectContainerNode.getText() must not return null");
      assertFalse(text.isBlank(), "Grafted CICS text must not be blank");
      assertTrue(
          text.toUpperCase().contains("EXEC"),
          "Each grafted node must reproduce its original EXEC CICS block; got: " + text);
    }
  }

  /** Grafting must attach at the blanked position, i.e. under a {@code dialectNodeFiller}. */
  @Test
  void eachGraftedNodeHangsOffADialectNodeFillerContext() throws IOException {
    CobolEntityNavigator navigator = cicsPipeline().parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      String parentName =
          dcn.getParent() == null ? "null" : dcn.getParent().getClass().getSimpleName();
      assertEquals(
          "DialectNodeFillerContext",
          parentName,
          "Each DialectContainerNode must be a direct child of a DialectNodeFillerContext");
    }
  }

  // ---------- helpers ----------

  /**
   * Uses the pre-existing {@code smojol-test-code/cics.cbl}, which until now was referenced by no
   * test at all. Other regression tests in this module reach that shared fixture directory the same
   * way, by a {@code ../} relative path.
   */
  private ParsePipeline cicsPipeline() {
    SourceConfig sourceConfig =
        new SourceConfig(
            "cics.cbl",
            dir("../smojol-test-code"),
            ImmutableList.of(new File(dir("../smojol-test-code"))),
            "NONE");
    return new ParsePipeline(sourceConfig, makeOps(), LanguageDialect.COBOL);
  }

  private static ComponentsBuilder makeOps() {
    return new ComponentsBuilder(
        new CobolTreeVisualiser(),
        new EntityNavigatorBuilder(),
        new UnresolvedReferenceThrowStrategy(),
        new OccursIgnoringFormat1DataStructureBuilder(),
        new UUIDProvider(),
        new LocalFilesystemOperations());
  }

  private static String dir(String path) {
    return java.nio.file.Paths.get(System.getProperty("user.dir"), path).toString();
  }
}
