package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.graph.TypedGraphEdge;
import com.mojo.algorithms.graph.TypedGraphVertex;
import org.jgrapht.Graph;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import com.mojo.algorithms.graph.NamespaceQualifier;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.GraphMLExportConfig;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.analysis.task.analysis.BuildBaseModelTask;
import org.smojol.toolkit.analysis.task.analysis.ExportToGraphMLTask;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultError;
import com.mojo.algorithms.task.AnalysisTaskResultOK;

import java.io.File;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class ExportToGraphMLTaskRegressionTest {
    @Test
    void canCreateDataStructures() {
        SourceConfig sourceConfig = new SourceConfig("test-exp.cbl",
                "../smojol-test-code",
                ImmutableList.of(new File("../smojol-test-code")),
                "../che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar");
        LocalFilesystemOperations resourceOperations = new LocalFilesystemOperations();
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(), new UUIDProvider(), resourceOperations);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, LanguageDialect.COBOL);
        AnalysisTaskResult buildSeedModelResult = new BuildBaseModelTask(pipeline, new UUIDProvider()).run();
        switch (buildSeedModelResult) {
            case AnalysisTaskResultOK o -> {
                BaseAnalysisModel r = o.getDetail();
                GraphMLExportConfig graphMLOutputConfig = new GraphMLExportConfig(Path.of("test-code/out"), "test-graphml.json");
                NodeSpecBuilder qualifier = new NodeSpecBuilder(new NamespaceQualifier("SOME_SPACE"));
                AnalysisTaskResult result = new ExportToGraphMLTask(r.flowRoot(), r.dataStructures(), graphMLOutputConfig, qualifier, resourceOperations).run();
                assertTrue(result.isSuccess());
                Graph<TypedGraphVertex, TypedGraphEdge> root = ((AnalysisTaskResultOK) result).getDetail();
            }
            case AnalysisTaskResultError e -> {
                e.getException().printStackTrace();
                fail();
            }
        }
    }
}
