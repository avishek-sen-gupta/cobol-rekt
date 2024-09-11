package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.AccessChain;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

import java.util.List;

public class TableCallExpression extends CobolExpression {
    @Getter private final VariableExpression variableExpression;
    @Getter private final List<CobolExpression> indexes;

    public TableCallExpression(VariableExpression variableExpression, List<CobolParser.ArithmeticExpressionContext> indexContexts) {
        super(ImmutableList.of(variableExpression), "INDEX");
        this.variableExpression = variableExpression;
        CobolExpressionBuilder expressionBuilder = new CobolExpressionBuilder();
        this.indexes = indexContexts.stream().map(expressionBuilder::arithmetic).toList();
        children.addAll(indexes);
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return new PrimitiveCobolExpression(reference(data).getValue());
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + variableExpression.description()
                + String.join(", ", indexes.stream().map(CobolExpression::description).toList()) + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return variableExpression.expressionType(dataStructures);
    }

    @Override
    public CobolDataStructure reference(CobolDataStructure data) {
        String rootVariableName = variableExpression.getName();
        AccessChain chain = data.chain(rootVariableName);
        List<Integer> resolvedIndices = indexes.stream().map(index -> resolve(data, index)).toList();
        List<Integer> fixedIndices = resolvedIndices.stream().map(i -> i == 0 ? 1 : i).toList();
        return chain.run(fixedIndices);
    }

    private static int resolve(CobolDataStructure data, CobolExpression index) {
        return (int) index.evalAsNumber(data);
    }
}
