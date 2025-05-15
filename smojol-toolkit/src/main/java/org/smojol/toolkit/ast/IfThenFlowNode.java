package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.domain.SemanticCategory;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.vm.stack.StackFrames;

import java.util.ArrayList;
import java.util.List;

public class IfThenFlowNode extends CompositeCobolFlowNode {
    public IfThenFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.IF_YES;
    }

    @Override
    public String label() {
        return "Yes";
    }

    @Override
    public String name() {
        return "Yes";
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
        visitor.visitParentChildLink(this, internalTreeRoot, new VisitContext(level), nodeService, CHILD_IS_CONDITIONAL_STATEMENT);
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        List<ParseTree> children = new ArrayList<>();
        for (int i = 0; i < executionContext.getChildCount(); i++) {
            if (!(executionContext.getChild(i) instanceof CobolParser.ConditionalStatementCallContext)) continue;
            children.add(executionContext.getChild(i));
        }
        return children;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.DECISION_BRANCH);
    }
}
