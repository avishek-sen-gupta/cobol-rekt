package org.smojol.common.vm.reference;

import org.smojol.common.pseudocode.UnresolvedSymbolReferenceException;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

public class IntermediateExpressionReference implements CobolReference {
    private final CobolExpression expression;
    private final CobolDataStructure data;

    public IntermediateExpressionReference(CobolExpression expression, CobolDataStructure data) {
        this.expression = expression;
        this.data = data;
    }

    @Override
    public TypedRecord resolveAs(AbstractCobolType type) {
        return ConversionStrategy.convert(internalResolve(), type);
    }

    @Override
    public CobolDataStructure resolve() {
        return new DetachedDataStructure(internalResolve());
    }

    @Override
    public CobolExpression asExpression() {
        return expression;
    }

    private TypedRecord internalResolve() {
        CobolExpression expressionResult = expression.evaluate(data);
        if (expressionResult instanceof PrimitiveCobolExpression p)
            return p.data();

        throw new UnresolvedSymbolReferenceException(expressionResult);
    }

    @Override
    public void set(CobolReference rhs) {
        throw new UnsupportedOperationException("Cannot reference intermediate expressions");
    }
}
