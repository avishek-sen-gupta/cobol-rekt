package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.domain.SemanticCategory;
import java.util.List;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

public class StopFlowNode extends CobolFlowNode {
  public StopFlowNode(
      ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
    super(parseTree, scope, nodeService, stackFrames);
  }

  @Override
  public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
    return interpreter.scope(this).executeExit(this, nodeService);
  }

  @Override
  public FlowNodeType type() {
    return FlowNodeType.STOP;
  }

  @Override
  public List<SemanticCategory> categories() {
    return ImmutableList.of(SemanticCategory.CONTROL_FLOW);
  }
}
