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

import java.util.ArrayList;
import java.util.List;

@Getter
public class SubtractFlowNode extends CobolFlowNode {
    private List<CobolParser.SubtractSubtrahendContext> subtrahends;
    private List<CobolParser.SubtractMinuendContext> minuends;
    private CobolParser.SubtractMinuendGivingContext minuendsGiving;
    private List<CobolParser.SubtractSubtrahendContext> subtrahendsGiving;
    private List<CobolParser.SubtractGivingContext> givingDestinations;
    private final List<CobolExpression> destinationExpressions = new ArrayList<>();
    private final List<CobolExpression> subtrahendExpressions = new ArrayList<>();
    private final List<CobolExpression> minuendExpressions = new ArrayList<>();

    public SubtractFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.SubtractStatementContext subtractStatement = new SyntaxIdentity<CobolParser.SubtractStatementContext>(executionContext).get();
        if (subtractStatement.subtractFromStatement() != null) {
            minuends = subtractStatement.subtractFromStatement().subtractMinuend();
            subtrahends = subtractStatement.subtractFromStatement().subtractSubtrahend();
            minuendsGiving = null;
            subtrahendsGiving = ImmutableList.of();
            givingDestinations = ImmutableList.of();
        }
        else if (subtractStatement.subtractFromGivingStatement() != null) {
            minuends = ImmutableList.of();
            subtrahends = ImmutableList.of();
            minuendsGiving = subtractStatement.subtractFromGivingStatement().subtractMinuendGiving();
            subtrahendsGiving = subtractStatement.subtractFromGivingStatement().subtractSubtrahend();
            givingDestinations = subtractStatement.subtractFromGivingStatement().subtractGiving();
        }
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.SUBTRACT;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeSubtract(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public boolean isMergeable() {
        return true;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.COMPUTATIONAL, SemanticCategory.DATA_FLOW);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();

        if (!isGiving()) {
            List<CobolExpression> lhsExpressions = minuends.stream().map(lhs -> builder.identifier(lhs.generalIdentifier())).toList();
            List<CobolExpression> rhsExpressions = subtrahends.stream().map(rhs -> builder.literalOrIdentifier(rhs.literal(), rhs.generalIdentifier())).toList();
            subtrahendExpressions.addAll(rhsExpressions);
            minuendExpressions.addAll(lhsExpressions);
            destinationExpressions.addAll(lhsExpressions);
        } else {
            CobolExpression lhsGivingExpressions = builder.literalOrIdentifier(minuendsGiving.literal(), minuendsGiving.generalIdentifier());
            List<CobolExpression> rhsGivingExpressions = subtrahendsGiving.stream().map(rhs -> builder.literalOrIdentifier(rhs.literal(), rhs.generalIdentifier())).toList();
            List<CobolExpression> givingDestinationExpressions = givingDestinations.stream().map(dest -> builder.identifier(dest.generalIdentifier())).toList();
            subtrahendExpressions.addAll(rhsGivingExpressions);
            minuendExpressions.add(lhsGivingExpressions);
            destinationExpressions.addAll(givingDestinationExpressions);
        }
    }

    public boolean isGiving() {
        CobolParser.SubtractStatementContext subtractStatement = new SyntaxIdentity<CobolParser.SubtractStatementContext>(executionContext).get();
        return subtractStatement.subtractFromGivingStatement() != null;
    }
}
