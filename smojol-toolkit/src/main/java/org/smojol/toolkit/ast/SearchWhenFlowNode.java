package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.CobolExpressionBuilder;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

@Getter
public class SearchWhenFlowNode extends CompositeCobolFlowNode {
    private FlowNode condition;
    private CobolExpression conditionExpression;
    private List<FlowNode> whenFlowNodes;

    public SearchWhenFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.SearchWhenContext searchWhenStatementContext = (CobolParser.SearchWhenContext) executionContext;
        condition = nodeService.node(searchWhenStatementContext.condition(), this, staticFrameContext);
        condition.buildFlow();
        super.buildInternalFlow();
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        CobolParser.SearchWhenContext searchWhenStatementContext = (CobolParser.SearchWhenContext) executionContext;
        return searchWhenStatementContext.conditionalStatementCall();
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        condition.acceptUnvisited(visitor, level);
        // Condition is already shown in parent's label, so we don't need to explicitly visit (and create) a condition graph node
//        visitor.visitParentChildLink(this, condition, nodeService);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.SEARCH_WHEN;
    }

    @Override
    public String name() {
        CobolParser.SearchWhenContext searchWhenStatementContext = (CobolParser.SearchWhenContext) executionContext;
        return "When\n" + NodeText.originalText(searchWhenStatementContext.condition(), NodeText::PASSTHROUGH);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        CobolParser.SearchWhenContext searchWhenStatementContext = (CobolParser.SearchWhenContext) executionContext;
        conditionExpression = new CobolExpressionBuilder().condition(searchWhenStatementContext.condition(), nodeService.getDataStructures());
        whenFlowNodes = searchWhenStatementContext.conditionalStatementCall().stream().map(stmt -> nodeService.node(stmt, this, staticFrameContext)).toList();
        whenFlowNodes.forEach(stmt -> stmt.resolve(symbolTable, dataStructures));
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.SEARCH);
    }
}
