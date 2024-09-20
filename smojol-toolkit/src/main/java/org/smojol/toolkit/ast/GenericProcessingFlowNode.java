package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolDataDivisionParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

public class GenericProcessingFlowNode implements FlowNode {
    @Getter
    private final Class<? extends FlowNode> type;
    private List<FlowNode> nodes = new ArrayList<>();
    private final FlowNode enclosingScope;
    private String uuid;

    public GenericProcessingFlowNode(FlowNode node, FlowNode enclosingScope, FlowNodeService nodeService) {
        this.uuid = nodeService.nextID();
        this.enclosingScope = enclosingScope;
        nodes.add(node);
        type = node.getClass();
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
        nodes.add(successor);
    }

    @Override
    public String name() {
        return "Processing Block: " + uuid + label();
    }

    @Override
    public String originalText() {
        return label();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.GENERIC_PROCESSING;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.CODE_BLOCK);
    }

    @Override
    public CodeSentinelType codeSentinelType() {
        return CodeSentinelType.BODY;
    }

    @Override
    public void accept(FlowNodeVisitor visitor, int level) {

    }

    @Override
    public void accept(FlowNodeVisitor visitor, FlowNodeCondition stopCondition, int level) {

    }

    @Override
    public List<? extends ParseTree> getChildren() {
        return List.of();
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {

    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {

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
        return new CobolDataDivisionParser.LabelRecordsClauseContext(null, 1);
    }

    @Override
    public void addIncomingNode(FlowNode flowNode) {
    }

    @Override
    public boolean accessesDatabase() {
        return false;
    }

    @Override
    public boolean isMergeable() {
        return false;
    }

    public void append(FlowNode vanillaNode) {

    }

    public void add(FlowNode node) {
        nodes.add(node);
    }
    @Override
    public String toString() {
        return label() + "/" + uuid;
    }

    public boolean contains(FlowNode node) {
        return nodes.contains(node) || nodes.stream().anyMatch(n -> n.contains(node));
    }

    @Override
    public List<FlowNode> getOutgoingNodes() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<FlowNode> getIncomingNodes() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String label() {
        StringBuilder builder = new StringBuilder("Processing\n------------------------\n");
        nodes.forEach(n -> builder.append(NodeText.originalText(n.getExecutionContext(), NodeText::PASSTHROUGH)).append("\n"));
        return builder.toString();
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
        return CobolVmSignal.CONTINUE;
    }

    @Override
    public void addComment(CommentBlock cb) {

    }

    @Override
    public List<CommentBlock> getCommentBlocks() {
        return ImmutableList.of();
    }

    @Override
    public void addChild(FlowNode child) {

    }

    @Override
    public void buildTwin() {

    }

    @Override
    public String id() {
        return name() + "/" + uuid;
    }

    @Override
    public FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete) {
        return null;
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
