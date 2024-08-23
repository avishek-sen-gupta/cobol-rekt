package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
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
        return FlowNodeType.CICS;
    }

    @Override
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.CONTROL_FLOW);
    }
}
