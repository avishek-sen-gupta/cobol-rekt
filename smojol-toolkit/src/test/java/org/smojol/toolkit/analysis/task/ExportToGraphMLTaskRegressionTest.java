package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.GraphMLExportConfig;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.analysis.task.analysis.BuildSeedModelTask;
import org.smojol.toolkit.analysis.task.analysis.ExportToGraphMLTask;
import org.smojol.toolkit.ast.FlowchartBuilderImpl;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultError;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

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
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(), FlowchartBuilderImpl::build,
                new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(), new UUIDProvider(), resourceOperations);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, LanguageDialect.COBOL);
        AnalysisTaskResult buildSeedModelResult = new BuildSeedModelTask(pipeline, new UUIDProvider()).run();
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
