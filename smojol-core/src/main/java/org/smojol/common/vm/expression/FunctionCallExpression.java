package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.reference.DetachedDataStructure;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

public class FunctionCallExpression extends CobolExpression {
    private final DetachedDataStructure proxyReturnValue;
    private final String functionName;
    private final List<CobolExpression> arguments;

    public FunctionCallExpression(CobolParser.FunctionCallContext functionCallContext) {
        super(ImmutableList.of());
        functionName = functionCallContext.functionName().getText();
        arguments = functionCallContext.argument().stream().map(arg -> {
            ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
            arg.arithmeticExpression().accept(arithmeticExpressionVisitor);
            return arithmeticExpressionVisitor.getExpression();
        }).toList();
        proxyReturnValue = new DetachedDataStructure(TypedRecord.typedNumber(1));
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return new PrimitiveCobolExpression(proxyReturnValue.getValue());
    }
}
