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
import org.smojol.common.vm.type.AbstractCobolType;

import java.util.List;
import java.util.Optional;

@Getter
public class MoveFlowNode extends CobolFlowNode {
    private CobolParser.MoveToSendingAreaContext fromSingle;
    private List<CobolParser.GeneralIdentifierContext> tos;
    private List<CobolExpression> fromExpressions;
    private List<CobolExpression> toExpressions;

    public MoveFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    // TODO: Support CORRESPONDING, fromSingle only looks at MoveTo
    @Override
    public void buildInternalFlow() {
        CobolParser.MoveStatementContext moveStatement = new SyntaxIdentity<CobolParser.MoveStatementContext>(executionContext).get();
        fromSingle = Optional.of(moveStatement)
                .map(CobolParser.MoveStatementContext::moveToStatement)
                .map(CobolParser.MoveToStatementContext::moveToSendingArea).orElse(null);
        tos = moveStatement.moveToStatement() != null
                ? moveStatement.moveToStatement().generalIdentifier()
                : moveStatement.moveCorrespondingToStatement().generalIdentifier();
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.MOVE;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeMove(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public boolean isMergeable() {
        return true;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.DATA_FLOW);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        CobolParser.MoveStatementContext moveStatement = new SyntaxIdentity<CobolParser.MoveStatementContext>(executionContext).get();
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        toExpressions = toExpressions(moveStatement, builder);
        if (moveStatement.moveToStatement() != null) {
            CobolParser.MoveToSendingAreaContext sendingArea = moveStatement.moveToStatement().moveToSendingArea();
            AbstractCobolType expectedType = toExpressions.getFirst().expressionType(dataStructures);
            // TODO: Maybe distribute this across multiple expressions, one corresponding to each destination, but with a separate type
            fromExpressions = ImmutableList.of(sendingArea.literal() != null ? builder.literal(sendingArea.literal(), expectedType) : builder.identifier(sendingArea.generalIdentifier()));
        } else {
            CobolParser.MoveCorrespondingToSendingAreaContext sendingArea = moveStatement.moveCorrespondingToStatement().moveCorrespondingToSendingArea();
            fromExpressions = ImmutableList.of(builder.identifier(sendingArea.generalIdentifier()));
        }
    }

    private static List<CobolExpression> toExpressions(CobolParser.MoveStatementContext moveStatement, CobolExpressionBuilder builder) {
        if (moveStatement.moveToStatement() != null)
            return moveStatement.moveToStatement().generalIdentifier().stream().map(builder::identifier).toList();
        else
            return moveStatement.moveCorrespondingToStatement().generalIdentifier().stream().map(builder::identifier).toList();
    }
}
