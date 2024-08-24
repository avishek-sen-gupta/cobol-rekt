package org.smojol.toolkit.analysis.graph.graphml;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;
import org.smojol.toolkit.analysis.graph.jgrapht.JGraphTCodeOperations;

import static com.mojo.woof.NodeRelations.CONTAINS_CODE;

public class JGraphTASTGraphBuilderVisitor extends FlowNodeASTVisitor<FlowNode> {
    private final JGraphTCodeOperations astGraphOperations;

    public JGraphTASTGraphBuilderVisitor(JGraphTCodeOperations astGraphOperations, FlowNode ancestor) {
        super(ancestor);
        this.astGraphOperations = astGraphOperations;
    }

    public JGraphTASTGraphBuilderVisitor(JGraphTCodeOperations astGraphOperations) {
        this(astGraphOperations, null);
    }

    @Override
    public FlowNodeASTVisitor<FlowNode> visit(FlowNode node) {
        astGraphOperations.addNode(node);
        if (ancestor == null) return new JGraphTASTGraphBuilderVisitor(astGraphOperations, node);
        astGraphOperations.connect(ancestor, node, CONTAINS_CODE);
        return new JGraphTASTGraphBuilderVisitor(astGraphOperations, node);
    }
}
