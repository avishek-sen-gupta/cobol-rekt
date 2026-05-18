package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.domain.SemanticCategory;
import java.util.List;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.stack.StackFrames;

public class PerformTestFlowNode extends CobolFlowNode {
  public PerformTestFlowNode(
      ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
    super(parseTree, scope, nodeService, stackFrames);
  }

  @Override
  public FlowNodeType type() {
    return FlowNodeType.PERFORM_TEST;
  }

  @Override
  public String label() {
    return originalText();
  }

  @Override
  public List<SemanticCategory> categories() {
    return ImmutableList.of(SemanticCategory.DECISION);
  }
}
