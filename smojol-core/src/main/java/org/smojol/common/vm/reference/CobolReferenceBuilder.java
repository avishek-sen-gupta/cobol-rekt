package org.smojol.common.vm.reference;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.*;
import org.smojol.common.vm.structure.AccessChain;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class CobolReferenceBuilder {

    public CobolReference getReference(CobolParser.GeneralIdentifierContext to, CobolDataStructure data) {
        return new VariableCobolReference(resolve(to, data));
    }

    public CobolDataStructure resolve(CobolParser.GeneralIdentifierContext to, CobolDataStructure data) {
        CobolParser.QualifiedDataNameContext qualifiedDataNameContext = to.qualifiedDataName();
        return resolve(qualifiedDataNameContext, data);
    }

    public CobolReference getShallowReference(String variableUsage, CobolDataStructure data) {
        return new VariableCobolReference(data.reference(variableUsage));
    }

    private static CobolDataStructure resolve(CobolParser.QualifiedDataNameContext qualifiedDataNameContext, CobolDataStructure data) {
        CobolDataStructure reference = data.reference(qualifiedDataNameContext.variableUsageName().getText());
        if (qualifiedDataNameContext.tableCall() == null) return reference;

        // TODO: Might precompute this
        AccessChain chain = data.chain(qualifiedDataNameContext.variableUsageName().getText());
        List<CobolParser.ArithmeticExpressionContext> indices = qualifiedDataNameContext.tableCall().arithmeticExpression();
        List<Integer> resolvedIndices = indices.stream().map(index -> resolve(data, index)).toList();
        List<Integer> fixedIndices = resolvedIndices.stream().map(i -> i == 0 ? 1 : i).toList();

        CobolDataStructure struct = chain.run(fixedIndices);
        return struct;
    }

    private static int resolve(CobolDataStructure data, CobolParser.ArithmeticExpressionContext index) {
        CobolExpression evaluatedIndex = new CobolExpressionBuilder().arithmetic(index).evaluate(data);
        int tableIndex = (int) evaluatedIndex.evalAsNumber(data);
        return tableIndex;
    }

    public CobolReference getReference(PrimitiveCobolExpression value) {
        return new PrimitiveReference(value.data());
    }

    public CobolReference getReference(String usageName, CobolDataStructure data) {
        return new VariableCobolReference(data.reference(usageName));
    }

    public CobolReference getReference(CobolExpression expression, CobolDataStructure data) {
        return switch (expression) {
            case VariableExpression expr -> new VariableCobolReference(expr.reference(data));
            case TableCallExpression expr -> new VariableCobolReference(expr.reference(data));
            case PrimitiveCobolExpression expr -> new PrimitiveReference(expr.data());
            default -> new IntermediateExpressionReference(expression, data);
        };
    }
}
