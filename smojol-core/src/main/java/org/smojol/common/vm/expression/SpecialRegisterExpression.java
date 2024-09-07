package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

public class SpecialRegisterExpression extends CobolExpression {
    private final CobolExpression expression;

    public SpecialRegisterExpression(CobolParser.SpecialRegisterContext specialRegisterContext) {
        expression = new CobolExpressionBuilder().identifier(specialRegisterContext.generalIdentifier());
        children.add(expression);
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        // TODO: Replace this with proper variable resolution
        return new PrimitiveCobolExpression(TypedRecord.typedNumber(5));
    }
}
