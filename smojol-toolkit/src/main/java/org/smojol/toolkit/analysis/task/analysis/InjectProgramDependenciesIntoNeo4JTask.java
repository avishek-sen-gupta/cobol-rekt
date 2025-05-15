package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.GraphSDK;
import org.smojol.toolkit.analysis.pipeline.CobolProgramDependencyNeo4JVisitor;
import com.mojo.algorithms.task.AnalysisTaskResult;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.common.program.CobolProgram;

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
