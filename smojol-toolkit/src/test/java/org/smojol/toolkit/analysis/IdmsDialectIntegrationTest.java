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
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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
 * Integration tests for the IDMS dialect grafting mechanism.
 *
 * <p>The mechanism has two phases, correlated purely by document position — no marker text is
 * injected into the source:
 *
 * <ol>
 *   <li><em>Extraction</em> (che4z side): each IDMS DML statement is blanked out of the extended
 *       document length-preservingly (every non-space, non-newline character becomes a zero-width
 *       space), and the region it occupied is recorded in {@link PersistentData} together with the
 *       original IDMS parse tree. Because the blanking preserves length, the recorded start position
 *       is still the position the fragment occupied.
 *   <li><em>Reinjection</em> (smojol side): {@code DialectIntegratorListener} walks the final COBOL
 *       parse tree, and for each {@code dialectNodeFiller} context — the run of zero-width spaces
 *       the COBOL parser matched where the fragment used to be — claims the recorded fragment
 *       covering that context's start position and attaches a {@code DialectContainerNode} wrapper
 *       as a child.
 * </ol>
 *
 * <p>The one substitution that is <em>not</em> length-preserving is the {@code _IF_ } prefix the
 * visitor puts in front of a fragment carrying an IDMS {@code ON} path-status clause, so the COBOL
 * parser treats what follows as a {@code dialectIfStatment}. That shifts the filler five columns
 * right of the recorded position, which is why fragment lookup is a range test rather than an exact
 * match. {@code idms-multiline.cbl} exercises it.
 *
 * <p>These tests verify the reinjection half end-to-end. The extraction half is tested separately
 * in {@code TestPersistentDataExtraction} in the dialect-idms module.
 */
public class IdmsDialectIntegrationTest {

  @BeforeEach
  void resetPersistentData() {
    PersistentData.reset();
  }

  // ---------- baseline parse sanity ----------

  @Test
  void canParseWithCobolDialectAfterLspUpgrade() throws IOException {
    SourceConfig sourceConfig =
        new SourceConfig(
            "no-branches.cbl",
            dir("test-code/flow-ast"),
            ImmutableList.of(new File(dir("test-code/flow-ast"))),
            "NONE");

    ParsePipeline pipeline = new ParsePipeline(sourceConfig, makeOps(), LanguageDialect.COBOL);
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    assertNotNull(navigator, "Navigator must be returned after parse");
  }

  @Test
  void canParseIdmsCobolWithDialectReinjection() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    assertNotNull(navigator, "Navigator must be returned after IDMS parse");

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(),
        "Expected IDMS dialect container nodes to be re-injected into the parse tree");
  }

  // ---------- extraction ↔ reinjection counts ----------

  /**
   * idms-simple.cbl has 4 IDMS-extracted constructs: 1. PROTOCOL. MODE IS BATCH DEBUG. (in
   * IDMS-CONTROL SECTION) 2. BIND RUN-UNIT. 3. READY. 4. FINISH. Verifies exactly 4
   * DialectContainerNodes appear in the tree — one per recorded fragment, each claimed by the filler
   * context at its position.
   */
  @Test
  void dialectContainerNodeCountMatchesIdmsStatementCount() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(
        4,
        dialectNodes.size(),
        "Expected exactly 4 DialectContainerNodes reinjected — one per extracted IDMS construct");
  }

  // ---------- dialect annotation ----------

  /**
   * Every reinjected DialectContainerNode must carry LocalisedDialect.IDMS. This annotation is set
   * recursively by IdmsDialect.setDialectRecursively() during extraction and propagated into the
   * wrapper at reinjection time.
   */
  @Test
  void allReinjectedNodesCarryIdmsDialect() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      assertEquals(
          LocalisedDialect.IDMS,
          dcn.getDialect(),
          "Every DialectContainerNode must carry LocalisedDialect.IDMS");
    }
  }

  // ---------- text reconstruction ----------

  /**
   * getText() on each reinjected node must return the original IDMS DML text — non-empty, non-null,
   * and free of any {@code _DIALECT_} marker (the fork no longer emits one; this asserts no marker
   * ever leaks back in). This exercises NodeText.originalText() via DialectContainerNode.getText().
   */
  @Test
  void reinjectedNodesReturnNonEmptyOriginalText() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      String text = node.getText();
      assertNotNull(text, "DialectContainerNode.getText() must not return null");
      assertFalse(
          text.isBlank(),
          "DialectContainerNode.getText() must return non-empty original IDMS text");
      assertFalse(
          text.contains("_DIALECT_"),
          "getText() must return original IDMS text, not the placeholder marker; got: " + text);
    }
  }

  // ---------- tree structure ----------

  /**
   * The parent of every DialectContainerNode must be a DialectNodeFillerContext. This confirms that
   * reinjection attaches nodes exactly at the blanked-out fragment positions in the COBOL parse
   * tree, not at arbitrary locations.
   */
  @Test
  void eachDialectContainerNodeHasDialectNodeFillerParent() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      String parentName =
          dcn.getParent() == null ? "null" : dcn.getParent().getClass().getSimpleName();
      assertTrue(
          "DialectNodeFillerContext".equals(parentName),
          "Each DialectContainerNode must be a direct child of a DialectNodeFillerContext; got: "
              + parentName);
    }
  }

  /**
   * DialectContainerNode.getStart() and getStop() must return non-null tokens. Although these
   * tokens are synthetic (token type COMPUTATIONAL, no valid source offsets), null tokens would
   * cause NPEs in any downstream code that reads token ranges (e.g. error reporters, serialisers,
   * code-lens providers).
   */
  @Test
  void dialectContainerNodeHasNonNullStartAndStopTokens() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      assertNotNull(dcn.getStart(), "getStart() must not return null");
      assertNotNull(dcn.getStop(), "getStop() must not return null");
    }
  }

  /**
   * Every DialectContainerNode must have exactly one child (the original IDMS parse tree node).
   * This exercises DialectContainerNode.getChildCount() and getChild(0).
   */
  @Test
  void eachDialectContainerNodeHasExactlyOneChild() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertFalse(
        dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      assertEquals(
          1,
          dcn.getChildCount(),
          "DialectContainerNode must expose exactly one child (the wrapped IDMS node)");
      assertNotNull(
          dcn.getChild(0),
          "DialectContainerNode.getChild(0) must return the wrapped IDMS parse tree node");
    }
  }

  // ---------- multi-line fragments and the _IF_ prefix ----------

  /**
   * idms-multiline.cbl deliberately exercises what idms-simple.cbl cannot, because every construct in
   * idms-simple.cbl sits on a single line.
   *
   * <p>It has 5 IDMS-extracted constructs:
   *
   * <ol>
   *   <li>{@code IDMS-CONTROL SECTION.} + {@code PROTOCOL. MODE IS BATCH DEBUG.} — one fragment
   *       spanning two lines
   *   <li>{@code BIND RUN-UNIT.}
   *   <li>{@code READY.}
   *   <li>{@code FINISH TASK} + {@code ON ANY-STATUS} — one fragment spanning two lines, and the one
   *       carrying an {@code ON} path-status clause, so it gets the {@code _IF_ } prefix
   *   <li>{@code IX-EMP EMPTY} — the condition of an IDMS {@code IF <entity> EMPTY}
   * </ol>
   */
  @Test
  void multiLineIdmsFixtureReinjectsEveryFragment() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-multiline.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(
        5,
        dialectNodes.size(),
        "Expected exactly 5 DialectContainerNodes for idms-multiline.cbl — one per extracted IDMS"
            + " construct. A lower count means a fragment lost its graft anchor");

    for (ParseTree node : dialectNodes) {
      DialectContainerNode dcn = (DialectContainerNode) node;
      String parentName =
          dcn.getParent() == null ? "null" : dcn.getParent().getClass().getSimpleName();
      assertEquals(
          "DialectNodeFillerContext",
          parentName,
          "Each DialectContainerNode must be a direct child of a DialectNodeFillerContext");
      assertEquals(1, dcn.getChildCount(), "Each DialectContainerNode wraps exactly one IDMS node");
      assertEquals(LocalisedDialect.IDMS, dcn.getDialect());
      assertFalse(dcn.getText().isBlank(), "Reinjected text must be the original IDMS text");
    }
  }

  /**
   * The load-bearing {@code +} in {@code dialectNodeFiller : ZERO_WIDTH_SPACE+ ...}.
   *
   * <p>The lexer rule is {@code ZERO_WIDTH_SPACE: '​' ('​' | [ ])*}, whose continuation
   * class matches a literal space but never a newline, so a fragment blanked across N source lines
   * lexes as N separate ZERO_WIDTH_SPACE tokens. Without the {@code +}, a filler context would stop
   * at the first line and the remaining tokens would fail to match — dialect subtrees would silently
   * vanish from the tree rather than throwing.
   */
  @Test
  void multiLineFragmentProducesAMultiTokenFillerRun() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-multiline.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> fillers =
        navigator.findAllByCondition(
            n -> n.getClass() == CobolParser.DialectNodeFillerContext.class);
    assertFalse(fillers.isEmpty(), "Precondition: filler contexts must be present");

    long multiLineFillers =
        fillers.stream()
            .map(CobolParser.DialectNodeFillerContext.class::cast)
            .filter(
                ctx ->
                    ctx.ZERO_WIDTH_SPACE().size() >= 2
                        && ctx.getStop() != null
                        && ctx.getStop().getLine() > ctx.getStart().getLine())
            .count();
    assertEquals(
        2,
        multiLineFillers,
        "idms-multiline.cbl has exactly 2 fragments blanked across more than one line (the"
            + " IDMS-CONTROL SECTION/PROTOCOL pair and FINISH TASK/ON ANY-STATUS), each of which"
            + " must lex to 2+ ZERO_WIDTH_SPACE tokens matched by a single filler context");
  }

  /**
   * The {@code _IF_ } prefix path — the only substitution that is not length-preserving.
   *
   * <p>A fragment carrying an IDMS {@code ON} path-status clause is replaced by {@code "_IF_ "}
   * followed by the blanked text, so the COBOL parser sees a {@code dialectIfStatment} wrapping the
   * filler and keeps the trailing COBOL imperative statement. The five prefix columns push the filler
   * right of the position recorded during extraction, so an exact-position lookup would miss it and
   * the fragment would silently never be grafted. This asserts the shifted filler still claims its
   * fragment.
   */
  @Test
  void onClauseFragmentIsCorrelatedDespiteTheIfPrefixColumnShift() throws IOException {
    ParsePipeline pipeline = idmsPipeline("idms-multiline.cbl");
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    List<ParseTree> underDialectIf =
        dialectNodes.stream()
            .filter(
                n ->
                    n.getParent() != null
                        && n.getParent().getParent()
                            instanceof CobolParser.DialectIfStatmentContext)
            .toList();

    assertEquals(
        1,
        underDialectIf.size(),
        "Exactly one fragment in idms-multiline.cbl carries an ON clause, so exactly one"
            + " DialectContainerNode must sit under a dialectIfStatment");

    DialectContainerNode dcn = (DialectContainerNode) underDialectIf.get(0);
    String text = dcn.getText().toUpperCase();
    assertTrue(
        text.contains("FINISH TASK") && text.contains("ON ANY-STATUS"),
        "The _IF_ -prefixed fragment must reinject the original FINISH TASK / ON ANY-STATUS text;"
            + " got: "
            + dcn.getText());

    // The filler really is shifted right of the recorded fragment start, so an exact-position
    // lookup would have missed it and this fragment would never have been grafted.
    CobolParser.DialectNodeFillerContext filler =
        (CobolParser.DialectNodeFillerContext) dcn.getParent();
    int fillerLine = filler.getStart().getLine();
    int fillerChar = filler.getStart().getCharPositionInLine();
    PersistentData.Fragment fragment = PersistentData.fragmentAt(fillerLine, fillerChar);
    assertNotNull(
        fragment,
        "A recorded fragment must still cover the shifted filler position " + fillerLine + ":"
            + fillerChar);
    assertEquals(fillerLine, fragment.startLine, "Fragment and filler must start on the same line");
    assertTrue(
        fragment.startChar < fillerChar,
        "The _IF_ prefix must shift the filler right of the recorded start ("
            + fragment.startChar
            + " -> "
            + fillerChar
            + "), which is exactly why Fragment.covers is a range test");
  }

  // ---------- no-dialect baseline ----------

  /**
   * A pure COBOL file (no IDMS statements) parsed with LanguageDialect.COBOL must produce zero
   * DialectContainerNodes and zero extractions. This guards against accidental injection of nodes
   * into non-IDMS programs.
   */
  @Test
  void pureCobolFileHasNoDialectContainerNodes() throws IOException {
    SourceConfig sourceConfig =
        new SourceConfig(
            "no-branches.cbl",
            dir("test-code/flow-ast"),
            ImmutableList.of(new File(dir("test-code/flow-ast"))),
            "NONE");
    ParsePipeline pipeline = new ParsePipeline(sourceConfig, makeOps(), LanguageDialect.COBOL);
    CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> dialectNodes =
        navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(0, dialectNodes.size(), "Pure COBOL file must produce no DialectContainerNodes");
  }

  // ---------- sequential file parsing ----------

  /**
   * Parsing the same IDMS file twice back-to-back must produce the correct reinjected nodes on each
   * parse independently. ParsePipeline does not reset PersistentData between parses, so the second
   * parse's fragments are recorded alongside the first parse's. Reinjection still resolves correctly
   * because a fragment is claimed once and only fillers at a matching position can claim it — so the
   * second parse's fillers pick up the second parse's fragments, not the already-claimed ones.
   *
   * <p>idms-simple.cbl produces 4 recorded fragments per parse (PROTOCOL + BIND RUN-UNIT + READY +
   * FINISH).
   */
  @Test
  void sequentialParsesResolveNodesCorrectly() throws IOException {
    ParsePipeline first = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator nav1 = first.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> nodes1 = nav1.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(4, nodes1.size(), "First parse must produce 4 DialectContainerNodes");

    ParsePipeline second = idmsPipeline("idms-simple.cbl");
    CobolEntityNavigator nav2 = second.parse(DataStructureValidation.NO_BUILD);

    List<ParseTree> nodes2 = nav2.findAllByCondition(n -> n instanceof DialectContainerNode);
    assertEquals(
        4,
        nodes2.size(),
        "Second sequential parse must also produce 4 reinjected DialectContainerNodes");

    for (ParseTree node : nodes2) {
      assertEquals(
          LocalisedDialect.IDMS,
          ((DialectContainerNode) node).getDialect(),
          "All second-parse nodes must carry LocalisedDialect.IDMS");
    }

    for (ParseTree node : nodes2) {
      String text = node.getText();
      assertFalse(
          text == null || text.isBlank(),
          "Second-parse DialectContainerNode.getText() must be non-empty");
    }
  }

  // ---------- helpers ----------

  private ParsePipeline idmsPipeline(String fileName) {
    String dialectJarPath =
        java.nio.file.Paths.get(
                System.getProperty("user.dir"),
                "..",
                "che-che4z-lsp-for-cobol-integration",
                "server",
                "dialect-idms",
                "target",
                "dialect-idms.jar")
            .toString();

    SourceConfig sourceConfig =
        new SourceConfig(
            fileName,
            dir("test-code/idms"),
            ImmutableList.of(new File(dir("test-code/idms"))),
            dialectJarPath);

    return new ParsePipeline(sourceConfig, makeOps(), LanguageDialect.IDMS);
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
