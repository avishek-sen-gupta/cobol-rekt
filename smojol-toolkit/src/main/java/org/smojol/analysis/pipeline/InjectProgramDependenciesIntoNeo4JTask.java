package org.smojol.analysis.pipeline;

import com.mojo.woof.GraphSDK;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.visualisation.CobolProgram;

public class InjectProgramDependenciesIntoNeo4JTask {
    private final NodeSpecBuilder qualifier;
    private final GraphSDK graphSDK;

    public InjectProgramDependenciesIntoNeo4JTask(NodeSpecBuilder qualifier, GraphSDK graphSDK) {
        this.qualifier = qualifier;
        this.graphSDK = graphSDK;
    }

    public AnalysisTaskResult run(CobolProgram root) {
        root.accept(new CobolProgramDependencyNeo4JVisitor(qualifier, graphSDK));
        return AnalysisTaskResult.OK("INJECT_PROGRAM_DEPENDENCIES_INTO_NEO4J");
    }
}
