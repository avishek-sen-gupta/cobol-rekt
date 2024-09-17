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
public class MultiplyFlowNode extends CobolFlowNode {
    private CobolParser.MultiplyLhsContext lhs;
    private List<CobolParser.MultiplyRegularOperandContext> rhses;
    private CobolParser.MultiplyGivingOperandContext givingRhs;
    private List<CobolParser.MultiplyGivingResultContext> givingDestinations;
    private CobolExpression lhsExpression;
    private List<CobolExpression> rhsExpressions;
    private CobolExpression givingRhsExpression;
    private List<CobolExpression> givingDestinationExpressions;
    private final List<CobolExpression> destinationExpressions = new ArrayList<>();
    private final List<CobolExpression> sourceExpressions = new ArrayList<>();

    public MultiplyFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.MultiplyStatementContext multiplyStatement = new SyntaxIdentity<CobolParser.MultiplyStatementContext>(executionContext).get();
        lhs = multiplyStatement.multiplyLhs();
        if (multiplyStatement.multiplyRegular() != null) {
            rhses = multiplyStatement.multiplyRegular().multiplyRegularOperand();
            givingRhs = null;
            givingDestinations = ImmutableList.of();
        } else if (multiplyStatement.multiplyGiving() != null) {
            rhses = ImmutableList.of();
            givingRhs = multiplyStatement.multiplyGiving().multiplyGivingOperand();
            givingDestinations = multiplyStatement.multiplyGiving().multiplyGivingResult();
        }
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.MULTIPLY;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeMultiply(this, nodeService);
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
        lhsExpression = builder.literalOrIdentifier(lhs.literal(), lhs.generalIdentifier());
        rhsExpressions = rhses.stream().map(rhs -> builder.identifier(rhs.generalIdentifier())).toList();
        givingRhsExpression = builder.literalOrIdentifier(givingRhs.literal(), givingRhs.generalIdentifier());
        givingDestinationExpressions = givingDestinations.stream().map(rhs -> builder.identifier(rhs.generalIdentifier())).toList();

        if (givingDestinationExpressions.isEmpty()) {
            sourceExpressions.add(lhsExpression);
            destinationExpressions.addAll(rhsExpressions);
        } else {
            sourceExpressions.add(lhsExpression);
            sourceExpressions.add(givingRhsExpression);
            destinationExpressions.addAll(givingDestinationExpressions);
        }

    }

    public boolean isGiving() {
        CobolParser.MultiplyStatementContext multiplyStatement = new SyntaxIdentity<CobolParser.MultiplyStatementContext>(executionContext).get();
        return multiplyStatement.multiplyGiving() != null;
    }
}
