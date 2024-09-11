package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

public class SpecialRegisterExpression extends CobolExpression {
    @Getter private final FunctionCallExpression functionCall;

    public SpecialRegisterExpression(CobolParser.SpecialRegisterContext specialRegisterContext) {
        super("FUNCTION_" + (specialRegisterContext.ADDRESS() != null ? "ADDRESS" : "LENGTH"));
        CobolExpression argument = new CobolExpressionBuilder().identifier(specialRegisterContext.generalIdentifier());
        functionCall = new FunctionCallExpression(specialRegisterContext.ADDRESS() != null ? "ADDRESS" : "LENGTH", ImmutableList.of(argument));
        children.add(functionCall);
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return functionCall.evaluate(data);
        // TODO: Replace this with proper variable resolution
//        return new PrimitiveCobolExpression(TypedRecord.typedNumber(5));
    }

    @Override
    public String description() {
        return functionCall.description();
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.NUMBER;
    }
}
