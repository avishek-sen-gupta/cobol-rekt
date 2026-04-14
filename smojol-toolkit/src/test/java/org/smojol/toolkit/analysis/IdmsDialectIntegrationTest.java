package org.smojol.toolkit.analysis;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.jupiter.api.Disabled;
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

public class IdmsDialectIntegrationTest {
    @Test
    void canParseWithCobolDialectAfterLspUpgrade() throws IOException {
        SourceConfig sourceConfig = new SourceConfig(
                "no-branches.cbl",
                dir("test-code/flow-ast"),
                ImmutableList.of(new File(dir("test-code/flow-ast"))),
                "NONE");

        ComponentsBuilder ops = new ComponentsBuilder(
                new CobolTreeVisualiser(),
                new EntityNavigatorBuilder(),
                new UnresolvedReferenceThrowStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(),
                new UUIDProvider(),
                new LocalFilesystemOperations());

        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, LanguageDialect.COBOL);
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);
        ParserRuleContext tree = pipeline.getTree();

        assertNotNull(navigator, "Navigator must be returned after parse");
        assertNotNull(tree, "Parse tree must be produced");
    }

    /**
     * Tests IDMS dialect parsing with real IDMS COBOL source (BIND, OBTAIN, FINISH verbs).
     * Requires the IDMS dialect JAR to be discoverable by DialectDiscoveryService via Guice DI.
     * Currently disabled because the standalone test Guice context does not load the IDMS dialect
     * plugin from the JAR — the DialectService reports "IDMS dialect is missing".
     * This test is exercised end-to-end via the CLI instead.
     */
    @Test
    @Disabled("IDMS dialect JAR discovery requires full Guice DI wiring not available in unit tests")
    void canParseIdmsCobolWithDialectReinjection() throws IOException {
        SourceConfig sourceConfig = new SourceConfig(
                "flowgraph-idms.cbl",
                "../smojol-test-code",
                ImmutableList.of(new File("../smojol-test-code")),
                "../che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar");

        ComponentsBuilder ops = new ComponentsBuilder(
                new CobolTreeVisualiser(),
                new EntityNavigatorBuilder(),
                new UnresolvedReferenceThrowStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(),
                new UUIDProvider(),
                new LocalFilesystemOperations());

        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, LanguageDialect.IDMS);
        CobolEntityNavigator navigator = pipeline.parse(DataStructureValidation.NO_BUILD);
        ParserRuleContext tree = pipeline.getTree();

        assertNotNull(navigator, "Navigator must be returned after IDMS parse");
        assertNotNull(tree, "Parse tree must be produced for IDMS COBOL");

        List<ParseTree> dialectNodes = navigator.findAllByCondition(
                n -> n instanceof DialectContainerNode);
        assertFalse(dialectNodes.isEmpty(),
                "Expected IDMS dialect container nodes to be re-injected into the parse tree");
    }

    private static String dir(String path) {
        return java.nio.file.Paths.get(System.getProperty("user.dir"), path).toString();
    }
}
