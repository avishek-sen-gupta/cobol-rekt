package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.CobolExpressionBuilder;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

@Getter
public class IfFlowNode extends CobolFlowNode {
    private FlowNode ifThenBlock;
    private FlowNode ifElseBlock;
    @Getter private CobolParser.ConditionContext condition;
    @Getter private CobolExpression conditionExpression;

    public IfFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildControlFlow() {
        ifThenBlock.buildControlFlow();
        if (ifElseBlock != null) ifElseBlock.buildControlFlow();
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.IfStatementContext ifStatement = new SyntaxIdentity<CobolParser.IfStatementContext>(getExecutionContext()).get();
        condition = (CobolParser.ConditionContext) ifStatement.getChild(1);
        FlowNode ifThenBlock = nodeService.node(ifStatement.ifThen(), this, staticFrameContext.add(this));
        ifThenBlock.buildFlow();
        this.ifThenBlock = ifThenBlock;
        CobolParser.IfElseContext ifElseCtx = ifStatement.ifElse();
        if (ifElseCtx == null) return;
        FlowNode ifElseBlock = nodeService.node(ifElseCtx, this, staticFrameContext.add(this));
        ifElseBlock.buildFlow();
        this.ifElseBlock = ifElseBlock;
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        visitor.visitParentChildLink(this, ifThenBlock, new VisitContext(level), nodeService);
        if (ifElseBlock != null) visitor.visitParentChildLink(this, ifElseBlock, new VisitContext(level), nodeService);

        ifThenBlock.accept(visitor, level);
        if (ifElseBlock != null) ifElseBlock.accept(visitor, level);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.IF_BRANCH;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeIf(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public String name() {
        CobolParser.IfStatementContext ifStatement = new SyntaxIdentity<CobolParser.IfStatementContext>(getExecutionContext()).get();
        CobolParser.ConditionContext condition = (CobolParser.ConditionContext) ifStatement.getChild(1);
        String codeText = NodeText.originalText(condition, NodeText::PASSTHROUGH);
        return "IS \n" + truncated(codeText, 40) + "?\n";
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
        conditionExpression = new CobolExpressionBuilder().condition(condition, dataStructures);
        ifThenBlock.resolve(symbolTable, dataStructures);
        if (ifElseBlock != null) ifElseBlock.resolve(symbolTable, dataStructures);
    }
}
