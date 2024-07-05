package org.smojol.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNodeImpl;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.*;

public class CobolFlowNode implements FlowNode {
    protected final String uuid;
    protected List<FlowNode> outgoingNodes = new ArrayList<>();
    protected List<FlowNode> incomingNodes = new ArrayList<>();
    @Getter
    protected final ParseTree executionContext;
    protected FlowNodeService nodeService;
    private DomainDocument document = new DomainDocument();
    private boolean databaseAccess;
    protected FlowNode scope;
    protected final StackFrames staticFrameContext;

    public CobolFlowNode(ParseTree executionContext, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        this.uuid = String.valueOf(nodeService.counter());
        this.nodeService = nodeService;
        this.executionContext = executionContext;
        this.scope = scope;
        this.staticFrameContext = stackFrames;
    }

    @Override
    public void buildFlow() {
        System.out.println("Building flow for " + name());
        buildInternalFlow();
        buildOutgoingFlow();
    }

    @Override
    public void buildOutgoingFlow() {
        outgoingNodes.forEach(FlowNode::buildFlow);
    }

    @Override
    public void buildInternalFlow() {
    }

    @Override
    public void buildControlFlow() {
    }

    @Override
    public void goesTo(FlowNode successor) {
        outgoingNodes.add(successor);
        successor.addIncomingNode(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CobolFlowNode chartNode = (CobolFlowNode) o;
        return executionContext == chartNode.executionContext && scope == chartNode.scope;
    }

    @Override
    public int hashCode() {
        return Objects.hash(executionContext);
    }

    // Use only for debugging
    @Override
    public String toString() {
        if (executionContext instanceof ParserRuleContext) {
            return name() + "/" + ((ParserRuleContext) executionContext).getStart().getLine() + "/" + uuid;
        }
        return name() + "." + uuid;
    }

    @Override
    public String name() {
        if (executionContext.getClass() == CobolParser.ProcedureSectionContext.class)
            return ((CobolParser.ProcedureSectionContext) executionContext).procedureSectionHeader().sectionName().getText();
        if (executionContext.getClass() == CobolParser.ParagraphContext.class)
            return ((CobolParser.ParagraphContext) executionContext).paragraphDefinitionName().getText();
        if (executionContext.getClass() == CobolParser.StatementContext.class)
            return truncated(executionContext, 15);
        if (executionContext.getClass() == CobolParser.SentenceContext.class)
            return truncated(executionContext, 15);
//            return "SE:" + truncated(executionContext);
        if (executionContext.getClass() == TerminalNodeImpl.class)
            return executionContext.getText();
        if (executionContext.getClass() == CobolParser.ParagraphDefinitionNameContext.class)
            return "NAME: " + executionContext.getText();
        if (executionContext.getClass() == CobolParser.ProcedureSectionHeaderContext.class)
            return executionContext.getText();
        if (executionContext.getClass() == CobolParser.ParagraphsContext.class)
            return "para-group:";
        if (executionContext.getClass() == IdmsParser.IdmsStatementsContext.class)
            return truncated(executionContext, 15);
        return defaultName();
    }

    @Override
    public String originalText() {
        return NodeText.originalText(executionContext, NodeText::PASSTHROUGH);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.ATOMIC;
    }

    protected String defaultName() {
        return executionContext.getClass().getSimpleName() + "/" + uuid;
    }

    protected String truncated(ParseTree e, int truncationLimit) {
        return truncated(e.getText(), truncationLimit);
//        return e.getText().length() > truncationLimit ? e.getText().substring(0, truncationLimit) : e.getText();
    }

    protected String truncated(String s, int truncationLimit) {
        return s.length() > truncationLimit ? s.substring(0, truncationLimit) : s;
    }

    @Override
    public void accept(FlowNodeVisitor visitor, int level) {
        acceptUnvisited(visitor, level);
    }

    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
//        System.out.println("Current level: " + level);
        visitor.visit(this, outgoingNodes, incomingNodes, new VisitContext(level), nodeService);
        outgoingNodes.forEach(c -> c.accept(visitor, level));
    }

    @Override
    public void addIncomingNode(FlowNode flowNode) {
        incomingNodes.add(flowNode);
    }

    @Override
    public DomainDocument getNotes() {
        return document;
    }

    @Override
    public boolean accessesDatabase() {
        return databaseAccess;
    }

    @Override
    public boolean isMergeable() {
        return false;
    }

    @Override
    public boolean contains(FlowNode node) {
        return false;
    }

    @Override
    public List<FlowNode> getOutgoingNodes() {
        return outgoingNodes;
    }

    @Override
    public String label() {
        return name();
    }

    @Override
    public FlowNode passthrough() {
        return this;
    }

    @Override
    public boolean isPassthrough() {
        return false;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).execute(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    // Specifically to return if this node terminated further execution
    // Overrides of this might choose to continue based on specific signal
    // TODO: This should move to some sort of a state machine implementation
    protected CobolVmSignal continueOrAbort(CobolVmSignal defaultSignal, CobolInterpreter interpreter, FlowNodeService nodeService) {
        if (defaultSignal == CobolVmSignal.TERMINATE ||
                defaultSignal == CobolVmSignal.EXIT_PERFORM ||
                defaultSignal == CobolVmSignal.EXIT_SCOPE ||
                defaultSignal == CobolVmSignal.NEXT_SENTENCE) return defaultSignal;
        return next(defaultSignal, interpreter, nodeService);
    }

    protected CobolVmSignal next(CobolVmSignal signal, CobolInterpreter interpreter, FlowNodeService nodeService) {
        if (outgoingNodes.size() > 1) {
            System.out.println("WARNING: ROGUE NODE " + this.label());
        }
        if (outgoingNodes.isEmpty()) return signal;
        return outgoingNodes.getFirst().acceptInterpreter(interpreter, FlowControl::CONTINUE);
    }

    @Override
    public String id() {
        return name() + "." + uuid;
    }

    @Override
    public FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete) {
        if (this != startingNode && nodeCondition.apply(this)) return this;
        System.out.println("Num outgoing nodes: " + outgoingNodes.size());
        if (outgoingNodes.isEmpty()) return scope.next(nodeCondition, startingNode, true);
        for (FlowNode o : outgoingNodes) {
            FlowNode next = o.next(nodeCondition, startingNode, true);
            if (next != null) return next;
        }
        return null;
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
    }

    @Override
    public FlowNode findUpwards(FlowNodeCondition nodeCondition, FlowNode startingNode) {
        if (nodeCondition.apply(this)) return this;
        return scope.findUpwards(nodeCondition, startingNode);
    }

    @Override
    public FlowNode tail() {
        FlowNode current = this;
        while (!current.getOutgoingNodes().isEmpty()) {
            current = current.getOutgoingNodes().getFirst();
        }
        return current;
    }

    @Override
    public List<FlowNode> astChildren() {
        return List.of();
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        return ImmutableList.of();
    }
}
