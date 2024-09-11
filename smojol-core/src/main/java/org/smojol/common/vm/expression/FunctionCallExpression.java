package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.reference.DetachedDataStructure;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

@Getter
public class FunctionCallExpression extends CobolExpression {
    private final DetachedDataStructure proxyReturnValue;
    private final String functionName;
    private final List<CobolExpression> arguments;

    public FunctionCallExpression(CobolParser.FunctionCallContext functionCallContext) {
        this(functionCallContext.functionName().getText(), args(functionCallContext));
    }

    public FunctionCallExpression(String functionName, List<CobolExpression> arguments) {
        super(ImmutableList.of(), functionInfo(functionName));
        this.functionName = functionName;
        this.arguments = arguments;
        children.addAll(arguments);
        proxyReturnValue = new DetachedDataStructure(TypedRecord.typedNumber(1));
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return new PrimitiveCobolExpression(proxyReturnValue.getValue());
    }

    @Override
    public String description() {
        return functionInfo(functionName);
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.STRING;
    }

    private static String functionInfo(String fnName) {
        return "FUNCTION_" + fnName;
    }

    private static List<CobolExpression> args(CobolParser.FunctionCallContext functionCallContext) {
        CobolExpressionBuilder expressionBuilder = new CobolExpressionBuilder();
        return functionCallContext.argument().stream().map(arg -> expressionBuilder.arithmetic(arg.arithmeticExpression())).toList();
    }
}
