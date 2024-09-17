package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.*;
import org.smojol.common.idms.IdmsContainerNode;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class CicsBlockFlowNode extends CobolFlowNode {

    public CicsBlockFlowNode(IdmsContainerNode containerNode, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(containerNode, scope, nodeService, stackFrames);
    }

    @Override
    public String label() {
        return originalText();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.DIALECT_CONTAINER;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.TRANSACTION);
    }
}
