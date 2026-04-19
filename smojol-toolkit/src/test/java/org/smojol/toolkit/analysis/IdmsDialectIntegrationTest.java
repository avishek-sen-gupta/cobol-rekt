package org.smojol.toolkit.analysis;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.common.idms.DialectContainerNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.analysis.validation.DataStructureValidation;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for the IDMS dialect grafting mechanism.
 *
 * <p>The mechanism has two phases:
 * <ol>
 *   <li><em>Extraction</em> (che4z side): IDMS DML statements are replaced with
 *       {@code _DIALECT_ N .} placeholders in the preprocessed text; the original
 *       IDMS parse tree nodes are registered in {@link PersistentData} under the
 *       key {@code "IDMS-N"}.</li>
 *   <li><em>Reinjection</em> (smojol side): {@code DialectIntegratorListener} walks
 *       the final COBOL parse tree, finds each {@code dialectNodeFiller} context
 *       (the matched placeholder), looks up the original IDMS node from
 *       {@code PersistentData}, and attaches a {@code DialectContainerNode} wrapper
 *       as a child.</li>
 * </ol>
 *
 * <p>These tests verify the reinjection half end-to-end.
 * The extraction half is tested separately in
 * {@code TestPersistentDataExtraction} in the dialect-idms module.
 */
public class IdmsDialectIntegrationTest {

    @BeforeEach
    void resetPersistentData() {
        PersistentData.reset();
    }

    // ---------- baseline parse sanity ----------

    @Test
    void canParseWithCobolDialectAfterLspUpgrade() throws IOException {
        SourceConfig sourceConfig = new SourceConfig(
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

        List<ParseTree> dialectNodes = navigator.findAllByCondition(
                n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(),
                "Expected IDMS dialect container nodes to be re-injected into the parse tree");
    }

    // ---------- extraction ↔ reinjection counts ----------

    /**
     * idms-simple.cbl has 4 IDMS-extracted constructs:
     *   1. PROTOCOL. MODE IS BATCH DEBUG.  (in IDMS-CONTROL SECTION)
     *   2. BIND RUN-UNIT.
     *   3. READY.
     *   4. FINISH.
     * Verifies exactly 4 DialectContainerNodes appear in the tree.
     * Note: PersistentData.counter is NOT reset by ParsePipeline — it accumulates across parses.
     */
    @Test
    void dialectContainerNodeCountMatchesIdmsStatementCount() throws IOException {
        ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertEquals(4, dialectNodes.size(),
                "Expected exactly 4 DialectContainerNodes reinjected — one per extracted IDMS construct");
    }

    // ---------- dialect annotation ----------

    /**
     * Every reinjected DialectContainerNode must carry LocalisedDialect.IDMS.
     * This annotation is set recursively by IdmsDialect.setDialectRecursively() during extraction
     * and propagated into the wrapper at reinjection time.
     */
    @Test
    void allReinjectedNodesCarryIdmsDialect() throws IOException {
        ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

        for (ParseTree node : dialectNodes) {
            DialectContainerNode dcn = (DialectContainerNode) node;
            assertEquals(LocalisedDialect.IDMS, dcn.getDialect(),
                    "Every DialectContainerNode must carry LocalisedDialect.IDMS");
        }
    }

    // ---------- text reconstruction ----------

    /**
     * getText() on each reinjected node must return the original IDMS DML text — non-empty,
     * non-null, and must NOT contain the {@code _DIALECT_} placeholder marker.
     * This exercises NodeText.originalText() via DialectContainerNode.getText().
     */
    @Test
    void reinjectedNodesReturnNonEmptyOriginalText() throws IOException {
        ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

        for (ParseTree node : dialectNodes) {
            String text = node.getText();
            assertNotNull(text, "DialectContainerNode.getText() must not return null");
            assertFalse(text.isBlank(),
                    "DialectContainerNode.getText() must return non-empty original IDMS text");
            assertFalse(text.contains("_DIALECT_"),
                    "getText() must return original IDMS text, not the placeholder marker; got: " + text);
        }
    }

    // ---------- tree structure ----------

    /**
     * The parent of every DialectContainerNode must be a DialectNodeFillerContext.
     * This confirms that reinjection attaches nodes exactly at the placeholder positions
     * in the COBOL parse tree, not at arbitrary locations.
     */
    @Test
    void eachDialectContainerNodeHasDialectNodeFillerParent() throws IOException {
        ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

        for (ParseTree node : dialectNodes) {
            DialectContainerNode dcn = (DialectContainerNode) node;
            String parentName = dcn.getParent() == null ? "null" : dcn.getParent().getClass().getSimpleName();
            assertTrue("DialectNodeFillerContext".equals(parentName),
                    "Each DialectContainerNode must be a direct child of a DialectNodeFillerContext; got: " + parentName);
        }
    }

    /**
     * DialectContainerNode.getStart() and getStop() must return non-null tokens.
     * Although these tokens are synthetic (token type COMPUTATIONAL, no valid source offsets),
     * null tokens would cause NPEs in any downstream code that reads token ranges
     * (e.g. error reporters, serialisers, code-lens providers).
     */
    @Test
    void dialectContainerNodeHasNonNullStartAndStopTokens() throws IOException {
        ParsePipeline pipeline = idmsPipeline("idms-simple.cbl");
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

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

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(), "Precondition: at least one DialectContainerNode must be present");

        for (ParseTree node : dialectNodes) {
            DialectContainerNode dcn = (DialectContainerNode) node;
            assertEquals(1, dcn.getChildCount(),
                    "DialectContainerNode must expose exactly one child (the wrapped IDMS node)");
            assertNotNull(dcn.getChild(0),
                    "DialectContainerNode.getChild(0) must return the wrapped IDMS parse tree node");
        }
    }

    // ---------- no-dialect baseline ----------

    /**
     * A pure COBOL file (no IDMS statements) parsed with LanguageDialect.COBOL must produce
     * zero DialectContainerNodes and zero extractions.  This guards against accidental injection
     * of nodes into non-IDMS programs.
     */
    @Test
    void pureCobolFileHasNoDialectContainerNodes() throws IOException {
        SourceConfig sourceConfig = new SourceConfig(
                "no-branches.cbl",
                dir("test-code/flow-ast"),
                ImmutableList.of(new File(dir("test-code/flow-ast"))),
                "NONE");
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, makeOps(), LanguageDialect.COBOL);
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);

        List<ParseTree> dialectNodes = navigator.findAllByCondition(n -> n instanceof DialectContainerNode);
        assertEquals(0, dialectNodes.size(),
                "Pure COBOL file must produce no DialectContainerNodes");
    }

    // ---------- sequential file parsing ----------

    /**
     * Parsing two IDMS files back-to-back must produce the correct reinjected nodes for
     * each file independently. The counter accumulates across parses (no reset between files),
     * so the second parse extracts IDs 5-8; reinjection resolves them correctly because
     * both sets of entries remain in PersistentData.
     *
     * <p>idms-simple.cbl produces 4 extractions per parse (PROTOCOL + BIND RUN-UNIT + READY + FINISH).
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
        assertEquals(4, nodes2.size(),
                "Second sequential parse must also produce 4 reinjected DialectContainerNodes");

        for (ParseTree node : nodes2) {
            assertEquals(LocalisedDialect.IDMS, ((DialectContainerNode) node).getDialect(),
                    "All second-parse nodes must carry LocalisedDialect.IDMS");
        }

        for (ParseTree node : nodes2) {
            String text = node.getText();
            assertFalse(text == null || text.isBlank(),
                    "Second-parse DialectContainerNode.getText() must be non-empty");
        }
    }

    // ---------- helpers ----------

    private ParsePipeline idmsPipeline(String fileName) {
        String dialectJarPath = java.nio.file.Paths.get(System.getProperty("user.dir"),
                "..", "che-che4z-lsp-for-cobol-integration",
                "server", "dialect-idms", "target", "dialect-idms.jar").toString();

        SourceConfig sourceConfig = new SourceConfig(
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
