package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;
import org.smojol.analysis.pipeline.GraphDBTasks;
import org.smojol.analysis.pipeline.SmojolTasks;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.common.flowchart.*;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.interpreter.FlowASTOutputConfig;
import org.smojol.interpreter.FlowchartGenerationStrategy;
import org.smojol.interpreter.SourceConfig;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;

import static org.smojol.analysis.pipeline.AnalysisTask.*;

public class GraphExplorerMain {
    public static void main(String[] args) throws IOException {
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        new GraphDBTasks("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")},
                dialectJarPath, LanguageDialect.COBOL   , FlowchartGenerationStrategy.FULL_PROGRAM)
                .generateForPrograms(ImmutableList.of("test-exp.cbl"), ImmutableList.of(
                        INJECT_INTO_NEO4J,
                        EXPORT_TO_GRAPHML,
                        WRITE_FLOW_AST
                ));
    }
}
