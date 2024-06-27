package org.smojol.ast;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class CompositeCobolFlowNode extends CobolFlowNode {
    public static FlowNodeCondition CHILD_IS_CONDITIONAL_STATEMENT = node -> SyntaxIdentity.isOfType(node.getExecutionContext(), CobolParser.ConditionalStatementCallContext.class);
    protected FlowNode internalTreeRoot;

    public CompositeCobolFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        System.out.println("Building internal flow for " + name());
        List<? extends ParseTree> children = getChildren();
        if (children == null) return;
        System.out.println("Looking at " + name());
        internalTreeRoot = nodeService.node(children.getFirst(), this, staticFrameContext.add(this));
        FlowNode current = internalTreeRoot;
        for (int i = 0; i <= children.size() - 2; i++) {
            FlowNode nextNode = nodeService.node(children.get(i + 1), this, staticFrameContext.add(this));
            if (".".equals(nextNode.getExecutionContext().getText())) continue;
            FlowNode successor = nextNode;
            current.goesTo(successor);
            current = successor;
        }
        internalTreeRoot.buildFlow();
    }

    private boolean isNullDialectNode(FlowNode node) {
        ParseTree n = node.getExecutionContext();
        return n.getClass() == CobolParser.DialectNodeFillerContext.class
                && ((CobolParser.DialectNodeFillerContext) n).whatever() != null;
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        return ((ParserRuleContext) executionContext).children;
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        // super() call needs to happen first, otherwise duplicate overlays will be created, since children will get GenericProcessingNodes earlier than parents.
        super.acceptUnvisited(visitor, level);
        if (internalTreeRoot != null) {
            linkParentToChild(visitor, level);
            FlowNode current = internalTreeRoot;
            current.accept(visitor.newScope(this), level + 1);
        }
    }

    @Override
    public FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete) {
        System.out.println("Moved up to " + executionContext.getClass() + executionContext.getText());
        CobolEntityNavigator navigator = nodeService.getNavigator();
//        boolean shouldSearch = navigator.findByCondition(executionContext, n -> n == startingNode.getExecutionContext()) == null;
        if (!isComplete) {
            System.out.println("ITR is " + internalTreeRoot.getClass() + " " + internalTreeRoot);
            FlowNode searchResult = internalTreeRoot.next(nodeCondition, startingNode, false);
            if (searchResult != null) return searchResult;
        }

        FlowNode chainSearch = new FlowNodes(outgoingNodes, nodeService).first().next(nodeCondition, startingNode, false);
        if (chainSearch != null) return chainSearch;
        return scope != null ? scope.next(nodeCondition, startingNode, true) : new DummyFlowNode(nodeService, staticFrameContext);
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
        visitor.visitParentChildLink(this, internalTreeRoot, new VisitContext(level), nodeService);
    }

    @Override
    public FlowNodeType type() {
        if (executionContext.getClass() == CobolParser.ProcedureSectionContext.class) return FlowNodeType.SECTION;
        if (executionContext.getClass() == CobolParser.ParagraphContext.class) return FlowNodeType.PARAGRAPH;
        return FlowNodeType.COMPOSITE;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = acceptInterpreterForCompositeExecution(interpreter, flowControl);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    public CobolVmSignal acceptInterpreterForCompositeExecution(CobolInterpreter interpreter, FlowControl flowControl) {
        return executeInternalRoot(interpreter, nodeService);
    }

    protected CobolVmSignal executeInternalRoot(CobolInterpreter interpreter, FlowNodeService nodeService) {
        interpreter.enter(this, nodeService);
        CobolVmSignal signal = internalTreeRoot != null ? internalTreeRoot.acceptInterpreter(interpreter.scope(this), FlowControl::CONTINUE) : CobolVmSignal.CONTINUE;
        interpreter.exit(this, nodeService);
        return signal;
    }

    @Override
    public boolean contains(FlowNode node) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
