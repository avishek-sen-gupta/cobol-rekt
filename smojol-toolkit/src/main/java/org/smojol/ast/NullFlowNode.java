package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;

import java.util.List;

public class NullFlowNode implements FlowNode {

    private final int uuid;

    public NullFlowNode(FlowNodeService nodeService) {
        uuid = nodeService.counter();
    }

    @Override
    public void buildFlow() {

    }

    @Override
    public void buildOutgoingFlow() {

    }

    @Override
    public void buildInternalFlow() {

    }

    @Override
    public void buildControlFlow() {

    }

    @Override
    public void goesTo(FlowNode successor) {

    }

    @Override
    public void addIncomingNode(FlowNode flowNode) {

    }

    @Override
    public List<FlowNode> getOutgoingNodes() {
        return List.of();
    }

    @Override
    public FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete) {
        return null;
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {

    }

    @Override
    public void accept(FlowNodeVisitor visitor, int level) {

    }

    @Override
    public List<? extends ParseTree> getChildren() {
        return List.of();
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {

    }

    @Override
    public FlowNode findUpwards(FlowNodeCondition nodeCondition, FlowNode startingNode) {
        return null;
    }

    @Override
    public FlowNode tail() {
        return this;
    }

    @Override
    public List<FlowNode> astChildren() {
        return List.of();
    }

    @Override
    public ParseTree getExecutionContext() {
        return null;
    }

    @Override
    public DomainDocument getNotes() {
        return null;
    }

    @Override
    public boolean accessesDatabase() {
        return false;
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
    public String label() {
        return "";
    }

    @Override
    public String name() {
        return "";
    }

    @Override
    public String originalText() {
        return "NULL/" + id();
    }

    @Override
    public FlowNodeType type() {
        return null;
    }

    @Override
    public FlowNode passthrough() {
        return null;
    }

    @Override
    public boolean isPassthrough() {
        return false;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        return CobolVmSignal.CONTINUE;
    }

    @Override
    public String id() {
        return "NULL/" + uuid;
    }
}
