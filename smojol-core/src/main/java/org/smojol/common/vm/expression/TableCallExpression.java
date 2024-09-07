package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.AccessChain;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class TableCallExpression extends CobolExpression {
    private final VariableExpression variableExpression;
    private final List<CobolExpression> indexes;

    public TableCallExpression(VariableExpression variableExpression, List<CobolParser.ArithmeticExpressionContext> indexContexts) {
        super(ImmutableList.of(variableExpression));
        this.variableExpression = variableExpression;
        CobolExpressionBuilder expressionBuilder = new CobolExpressionBuilder();
        this.indexes = indexContexts.stream().map(expressionBuilder::arithmetic).toList();
        children.addAll(indexes);
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        String rootVariableName = variableExpression.name();
        AccessChain chain = data.chain(rootVariableName);
        List<Integer> resolvedIndices = indexes.stream().map(index -> resolve(data, index)).toList();
        List<Integer> fixedIndices = resolvedIndices.stream().map(i -> i == 0 ? 1 : i).toList();

        CobolDataStructure struct = chain.run(fixedIndices);
        return new PrimitiveCobolExpression(struct.getValue());
    }

    private static int resolve(CobolDataStructure data, CobolExpression index) {
        return (int) index.evalAsNumber(data);
    }
}
