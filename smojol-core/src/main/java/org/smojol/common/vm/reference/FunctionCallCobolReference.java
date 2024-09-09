package org.smojol.common.vm.reference;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.CobolExpressionBuilder;
import org.smojol.common.vm.expression.FunctionCallExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

public class FunctionCallCobolReference implements CobolReference {
    private final DetachedDataStructure proxyReturnValue;
    private final String functionName;
    private final List<CobolExpression> arguments;

    public FunctionCallCobolReference(CobolParser.FunctionCallContext functionCallContext) {
        functionName = functionCallContext.functionName().getText();
        arguments = functionCallContext.argument().stream().map(arg -> new CobolExpressionBuilder().arithmetic(arg.arithmeticExpression())).toList();
        proxyReturnValue = new DetachedDataStructure(TypedRecord.typedNumber(1));
    }

    @Override
    public TypedRecord resolveAs(AbstractCobolType type) {
        return proxyReturnValue.getValue();
    }

    @Override
    public CobolDataStructure resolve() {
        return proxyReturnValue;
    }

    @Override
    public CobolExpression asExpression() {
        return new FunctionCallExpression(functionName, arguments);
    }

    @Override
    public void set(CobolReference rhs) {
        throw new UnsupportedOperationException("Cannot reference intermediate expressions");
    }
}
