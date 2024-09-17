package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.ast.FlowNodeVisitor;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class DummyFlowNode extends CobolFlowNode {
    public DummyFlowNode(FlowNodeService nodeService, StackFrames stackFrames) {
        super(null, null, nodeService, stackFrames);
    }

    @Override
    public void buildFlow() {
        super.buildFlow();
    }

    @Override
    public String toString() {
        return "DUMMY_NODE";
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
//        super.acceptUnvisited(visitor, level);
    }

    @Override
    public String name() {
        return "DUMMY_EXECUTION_CONTEXT";
    }

    @Override
    public boolean equals(Object o) {
        return o == this;
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.DUMMY;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.PLACEHOLDER);
    }
}
