package org.smojol.toolkit.analysis;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.ParserRuleContext;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import com.mojo.algorithms.id.UUIDProvider;
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
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.assertNotNull;

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

    private static String dir(String path) {
        return Paths.get(System.getProperty("user.dir"), path).toString();
    }
}
