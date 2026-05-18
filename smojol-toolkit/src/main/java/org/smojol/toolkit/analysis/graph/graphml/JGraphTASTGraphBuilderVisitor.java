package org.smojol.toolkit.analysis.graph.graphml;

import static com.mojo.woof.NodeRelations.CONTAINS_CODE;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;
import org.smojol.toolkit.analysis.graph.jgrapht.JGraphTCodeOperations;

public class JGraphTASTGraphBuilderVisitor extends FlowNodeASTVisitor<FlowNode> {
  private final JGraphTCodeOperations astGraphOperations;

  public JGraphTASTGraphBuilderVisitor(
      JGraphTCodeOperations astGraphOperations, FlowNode ancestor) {
    super(ancestor);
    this.astGraphOperations = astGraphOperations;
  }

  public JGraphTASTGraphBuilderVisitor(JGraphTCodeOperations astGraphOperations) {
    this(astGraphOperations, null);
  }

  @Override
  public FlowNode visit(FlowNode node) {
    astGraphOperations.addNode(node);
    if (ancestor == null) return node;
    astGraphOperations.connect(ancestor, node, CONTAINS_CODE);
    return node;
  }

  @Override
  public FlowNodeASTVisitor<FlowNode> scope(FlowNode n, FlowNode visitResult) {
    return new JGraphTASTGraphBuilderVisitor(astGraphOperations, visitResult);
  }
}
