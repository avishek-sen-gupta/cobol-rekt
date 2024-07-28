package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCondition;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class SentenceFlowNode extends CompositeCobolFlowNode {
    @Override
    public FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete) {
        if (this != startingNode && nodeCondition.apply(this)) return this;
        if (outgoingNodes.isEmpty()) return scope.next(nodeCondition, startingNode, true);
        return outgoingNodes.getFirst().next(nodeCondition, startingNode, false);
    }

    public SentenceFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.SENTENCE;
    }

    @Override
    public boolean isMergeable() {
        CobolParser.SentenceContext e = (CobolParser.SentenceContext) executionContext;
        if (e.dialectStatement() != null) return false;
        long unmergeableStatementCount = e.statement().stream().filter(stmt -> !nodeService.existingNode(stmt).isMergeable()).count();
        return unmergeableStatementCount == 0;
//        return e.statement().size() == 1 && nodeService.node(e.statement(0)).isMergeable();
    }

    @Override
    public boolean contains(FlowNode node) {
        CobolParser.SentenceContext e = (CobolParser.SentenceContext) executionContext;
        List<CobolParser.StatementContext> statements = e.statement();
        return statements.stream().anyMatch(st -> st == node.getExecutionContext());
//        ChartNode current = internalTreeRoot;
//        return searchStatements(node, current);
    }

    // TODO: Why does the recursive search through the outgoingNodes of the internalTreeRoot collapse the graph?
    private boolean searchStatements(FlowNode searchTarget, FlowNode current) {
        if (current == searchTarget) return true;
        List<FlowNode> outgoingNodes = current.getOutgoingNodes();
        for (FlowNode outgoingNode : outgoingNodes) {
            boolean containsNode = searchStatements(searchTarget, outgoingNode);
            if (containsNode) return true;
        }
        return true;
    }

    @Override
    protected CobolVmSignal continueOrAbort(CobolVmSignal defaultSignal, CobolInterpreter interpreter, FlowNodeService nodeService) {
        if (defaultSignal == CobolVmSignal.TERMINATE ||
                defaultSignal == CobolVmSignal.EXIT_PERFORM ||
                defaultSignal == CobolVmSignal.EXIT_SCOPE) return defaultSignal;
        if (defaultSignal == CobolVmSignal.NEXT_SENTENCE) return next(CobolVmSignal.CONTINUE, interpreter, nodeService);
        return next(defaultSignal, interpreter, nodeService);
    }
}
