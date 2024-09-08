package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

public class SpecialRegisterExpression extends CobolExpression {

    private final FunctionCallExpression functionCall;

    public SpecialRegisterExpression(CobolParser.SpecialRegisterContext specialRegisterContext) {
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
}
