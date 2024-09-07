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
public class SubtractFlowNode extends CobolFlowNode {
    private List<CobolParser.SubtractSubtrahendContext> rhses;
    private List<CobolParser.SubtractMinuendContext> lhses;
    private CobolParser.SubtractMinuendGivingContext lhsGiving;
    private List<CobolParser.SubtractSubtrahendContext> rhsGiving;
    private List<CobolParser.SubtractGivingContext> givingDestinations;
    private List<CobolExpression> lhsExpressions;
    private List<CobolExpression> rhsExpressions;
    private List<CobolExpression> rhsGivingExpressions;
    private List<CobolExpression> givingDestinationExpressions;

    public SubtractFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.SubtractStatementContext subtractStatement = new SyntaxIdentity<CobolParser.SubtractStatementContext>(executionContext).get();
        if (subtractStatement.subtractFromStatement() != null) {
            lhses = subtractStatement.subtractFromStatement().subtractMinuend();
            rhses = subtractStatement.subtractFromStatement().subtractSubtrahend();
            lhsGiving = null;
            rhsGiving = ImmutableList.of();
            givingDestinations = ImmutableList.of();
        }
        else if (subtractStatement.subtractFromGivingStatement() != null) {
            lhses = ImmutableList.of();
            rhses = ImmutableList.of();
            lhsGiving = subtractStatement.subtractFromGivingStatement().subtractMinuendGiving();
            rhsGiving = subtractStatement.subtractFromGivingStatement().subtractSubtrahend();
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
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.COMPUTATIONAL, FlowNodeCategory.DATA_FLOW);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        lhsExpressions = lhses.stream().map(lhs -> builder.identifier(lhs.generalIdentifier())).toList();
        rhsExpressions = rhses.stream().map(rhs -> builder.literalOrIdentifier(rhs.literal(), rhs.generalIdentifier())).toList();
        rhsGivingExpressions = rhsGiving.stream().map(rhs -> builder.literalOrIdentifier(rhs.literal(), rhs.generalIdentifier())).toList();
        givingDestinationExpressions = givingDestinations.stream().map(dest -> builder.identifier(dest.generalIdentifier())).toList();
    }
}
