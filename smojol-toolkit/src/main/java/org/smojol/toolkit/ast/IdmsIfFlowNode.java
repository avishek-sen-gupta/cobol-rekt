package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class IdmsIfFlowNode extends CobolFlowNode {
    private FlowNode ifThenBlock;
    private FlowNode ifElseBlock;
    private FlowNode condition;

    public IdmsIfFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames staticFrameContext) {
        super(parseTree, scope, nodeService, staticFrameContext);
    }

    @Override
    public void buildInternalFlow() {
        ParseTree conditionContext = executionContext.getChild(1);
        condition = conditionChartNode(conditionContext);
        condition.buildFlow();
        CobolEntityNavigator navigator = nodeService.getNavigator();
        ParseTree ifThen = navigator.findByCondition(executionContext, n -> n.getClass() == CobolParser.IfThenContext.class, 1);
        ifThenBlock = nodeService.node(ifThen, this, staticFrameContext);
        ifThenBlock.buildFlow();
        ParseTree ifElse = navigator.findByCondition(executionContext, n -> n.getClass() == CobolParser.IfElseContext.class, 1);
        if (ifElse != null) {
            ifElseBlock = nodeService.node(ifElse, this, staticFrameContext);
            ifElseBlock.buildFlow();
        }
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        visitor.visitParentChildLink(this, condition, new VisitContext(level), nodeService);
        condition.accept(visitor, level);

        visitor.visitParentChildLink(this, ifThenBlock, new VisitContext(level), nodeService);
        ifThenBlock.accept(visitor, level);

        if (ifElseBlock != null) {
            visitor.visitParentChildLink(this, ifElseBlock, new VisitContext(level), nodeService);
            ifElseBlock.accept(visitor, level);
        }
    }

    private FlowNode conditionChartNode(ParseTree dialectStatementNode) {
        return nodeService.node(conditionContext(dialectStatementNode), this, staticFrameContext);
    }

    private ParseTree conditionContext(ParseTree searchRoot) {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        ParseTree idmsContainer = navigator.findByCondition(searchRoot, n -> n.getClass() == IdmsParser.IdmsIfStatementContext.class);
        return idmsContainer;
    }

    @Override
    public String name() {
        return "IDMS IF";
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.IF_BRANCH;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.DECISION);
    }

    @Override
    public List<FlowNode> astChildren() {
        return ifElseBlock != null ? ImmutableList.of(ifThenBlock, ifElseBlock) : ImmutableList.of(ifThenBlock);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        condition.resolve(symbolTable, dataStructures);
        ifThenBlock.resolve(symbolTable, dataStructures);
        if (ifElseBlock != null) ifElseBlock.resolve(symbolTable, dataStructures);
    }
}
