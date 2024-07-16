package org.smojol.analysis.graph;

import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;
import org.smojol.analysis.pipeline.SmojolPipeline;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.common.flowchart.*;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.interpreter.SourceConfig;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;

public class GraphExplorerMain {
    public static void main(String[] args) throws IOException {
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder());
        SourceConfig sourceConfig = new SourceConfig(
                new File("/Users/asgupta/code/smojol/smojol-test-code/test-exp.cbl"),
                new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")},
                "/Users/asgupta/code/smojol/out/test-cobol.json",
                dialectJarPath);
        ParsePipeline pipeline = new ParsePipeline(
                sourceConfig, ops, LanguageDialect.COBOL);

        new SmojolPipeline(NodeReferenceStrategy.NEW_AST_NODE, NodeReferenceStrategy.EXISTING_AST_NODE).run(pipeline);
    }
}
