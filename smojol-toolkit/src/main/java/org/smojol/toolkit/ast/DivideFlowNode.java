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
public class DivideFlowNode extends CobolFlowNode {
    private CobolParser.DivisorContext intoDivisor;
    private List<CobolParser.DivideIntoContext> dividends;
    private CobolParser.DivisorContext givingDividend;
    private CobolExpression divisorExpression;
    private List<CobolExpression> dividendExpressions;
    private List<CobolExpression> destinationExpressions;

    public DivideFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.DivideStatementContext divideStatement = new SyntaxIdentity<CobolParser.DivideStatementContext>(executionContext).get();
        CobolExpressionBuilder expressionBuilder = new CobolExpressionBuilder();
        if (divideStatement.divideIntoStatement() != null) {
            intoDivisor = divideStatement.divisor();
            dividends = divideStatement.divideIntoStatement().divideInto();
        } else if (divideStatement.divideByGivingStatement() != null) {
            intoDivisor = divideStatement.divideByGivingStatement().divisor();
            givingDividend = divideStatement.divisor();
        }
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.DIVIDE;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeDivide(this, nodeService);
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
        divisorExpression = builder.literalOrIdentifier(intoDivisor.literal(), intoDivisor.generalIdentifier());
        dividendExpressions = dividends != null ? dividends.stream().map(dividend -> builder.identifier(dividend.generalIdentifier())).toList()
                : ImmutableList.of(builder.literalOrIdentifier(givingDividend.literal(), givingDividend.generalIdentifier()));

        CobolParser.DivideStatementContext divideStatement = new SyntaxIdentity<CobolParser.DivideStatementContext>(executionContext).get();
        if (divideStatement.divideIntoStatement() != null) {
            // Format 1: See https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statements-divide-statement
            dividendExpressions = divideStatement.divideIntoStatement().divideInto().stream().map(lhs -> builder.identifier(lhs.generalIdentifier())).toList();
            divisorExpression = builder.literalOrIdentifier(divideStatement.divisor().literal(), divideStatement.divisor().generalIdentifier());
            destinationExpressions = dividendExpressions;
        } else if (divideStatement.divideIntoGivingStatement() != null) {
            // Format 2: See https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statements-divide-statement
            divisorExpression = builder.literalOrIdentifier(divideStatement.divisor().literal(), divideStatement.divisor().generalIdentifier());
            dividendExpressions = ImmutableList.of(builder.literalOrIdentifier(divideStatement.divideIntoGivingStatement().literal(),
                    divideStatement.divideIntoGivingStatement().generalIdentifier()));
            destinationExpressions = divideStatement.divideIntoGivingStatement().divideGivingPhrase()
                    .divideGiving().stream().map(dst -> builder.identifier(dst.generalIdentifier())).toList();

            // Format 4: See https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statements-divide-statement

        } else if (divideStatement.divideByGivingStatement() != null) {
            // Format 3: See https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statements-divide-statement
            divisorExpression = builder.literalOrIdentifier(divideStatement.divideByGivingStatement().divisor().literal(),
                    divideStatement.divideByGivingStatement().divisor().generalIdentifier());
            dividendExpressions = ImmutableList.of(builder.literalOrIdentifier(divideStatement.divisor().literal(), divideStatement.divisor().generalIdentifier()));
            destinationExpressions = divideStatement.divideByGivingStatement().divideGivingPhrase()
                    .divideGiving().stream().map(dst -> builder.identifier(dst.generalIdentifier())).toList();

            // Format 5: See https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statements-divide-statement
        }
    }

    public boolean isGiving() {
        CobolParser.DivideStatementContext divideStatement = new SyntaxIdentity<CobolParser.DivideStatementContext>(executionContext).get();
        return divideStatement.divideByGivingStatement() != null;
    }
}
