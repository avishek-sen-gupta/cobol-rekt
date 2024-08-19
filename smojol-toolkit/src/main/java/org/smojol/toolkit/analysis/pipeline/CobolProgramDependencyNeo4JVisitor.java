package org.smojol.toolkit.analysis.pipeline;

import com.mojo.woof.GraphSDK;
import org.neo4j.driver.Record;
import org.smojol.common.program.ProgramVisitor;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;
import org.smojol.common.program.CobolProgram;

public class CobolProgramDependencyNeo4JVisitor implements ProgramVisitor {
    private final Record neo4jRoot;
    private final GraphSDK graphSDK;
    private NodeSpecBuilder qualifier;

    public CobolProgramDependencyNeo4JVisitor(NodeSpecBuilder qualifier, GraphSDK graphSDK) {
        this(null, qualifier, graphSDK);
    }

    public CobolProgramDependencyNeo4JVisitor(CobolProgram root, NodeSpecBuilder qualifier, GraphSDK graphSDK) {
        this(root, null, qualifier, graphSDK);
    }

    public CobolProgramDependencyNeo4JVisitor(CobolProgram root, Record neo4jRoot, NodeSpecBuilder qualifier, GraphSDK graphSDK) {
        this.neo4jRoot = neo4jRoot;
        this.graphSDK = graphSDK;
        this.qualifier = qualifier;
    }

    @Override
    public ProgramVisitor visit(CobolProgram cobolProgram) {
        Record childNode = graphSDK.createNode(NodeToWoof.programToWoof(cobolProgram, qualifier));
        if (neo4jRoot != null) graphSDK.dependsUpon(neo4jRoot, childNode);
        return new CobolProgramDependencyNeo4JVisitor(cobolProgram, childNode, qualifier, graphSDK);
    }
}
