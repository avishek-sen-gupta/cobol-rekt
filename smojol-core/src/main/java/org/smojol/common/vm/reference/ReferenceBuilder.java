package org.smojol.common.vm.reference;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.LiteralResolver;
import org.smojol.common.vm.type.TypedRecord;

public class ReferenceBuilder {
    public CobolReference getReference(CobolParser.MoveToSendingAreaContext from, CobolDataStructure dataStructure) {
        if (from.generalIdentifier() != null) return getReference(from.generalIdentifier(), dataStructure);
        return new PrimitiveReference(typedValue(from));
    }

    private TypedRecord typedValue(CobolParser.MoveToSendingAreaContext from) {
        String v = new LiteralResolver().resolvedLiteral(from.literal());
//        if (v instanceof Double) return TypedRecord.typedNumber((Double) v);
        return TypedRecord.typedString(v.toString());
    }

    public CobolReference getReference(CobolParser.AddFromContext from, CobolDataStructure dataStructure) {
//        if (from.generalIdentifier() != null) return new VariableCobolReference(from.generalIdentifier().getText(), dataStructure);
        if (from.generalIdentifier() != null) return getReference(from.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(from.literal().getText()));
    }

    public CobolReference getReference(CobolParser.SubtractSubtrahendContext rhs, CobolDataStructure dataStructure) {
//        if (rhs.generalIdentifier() != null) return new VariableCobolReference(rhs.generalIdentifier().getText(), dataStructure);
        if (rhs.generalIdentifier() != null) return getReference(rhs.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(rhs.literal().getText()));
    }

    public CobolReference getReference(CobolParser.MultiplyLhsContext lhs, CobolDataStructure dataStructure) {
//        if (lhs.generalIdentifier() != null) return new VariableCobolReference(lhs.generalIdentifier().getText(), dataStructure);
        if (lhs.generalIdentifier() != null) return getReference(lhs.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(lhs.literal().getText()));
    }

    public CobolReference getReference(CobolParser.DivisorContext divisor, CobolDataStructure dataStructure) {
//        if (divisor.generalIdentifier() != null) return new VariableCobolReference(divisor.generalIdentifier().getText(), dataStructure);
        if (divisor.generalIdentifier() != null) return getReference(divisor.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(divisor.literal().getText()));
    }

    public CobolReference getReference(CobolParser.GeneralIdentifierContext to, CobolDataStructure data) {
        return new VariableCobolReference(resolve(to, data));
    }

    public CobolReference getReference(CobolParser.AddToContext to, CobolDataStructure data) {
        return getReference(to.generalIdentifier(), data);
    }

    public CobolReference getReference(CobolParser.SubtractMinuendContext lhs, CobolDataStructure data) {
        return getReference(lhs.generalIdentifier(), data);
    }

    public CobolReference getReference(CobolParser.QualifiedDataNameContext nameContext, CobolDataStructure data) {
        return new VariableCobolReference(resolve(nameContext, data));
    }

    public CobolDataStructure resolve(CobolParser.GeneralIdentifierContext to, CobolDataStructure data) {
        CobolParser.QualifiedDataNameContext qualifiedDataNameContext = to.qualifiedDataName();
        return resolve(qualifiedDataNameContext, data);
    }

    private static CobolDataStructure resolve(CobolParser.QualifiedDataNameContext qualifiedDataNameContext, CobolDataStructure data) {
        CobolDataStructure reference = data.reference(qualifiedDataNameContext.variableUsageName().getText());
        if (qualifiedDataNameContext.tableCall() == null) return reference;
        CobolParser.ArithmeticExpressionContext indexExpression = qualifiedDataNameContext.tableCall().arithmeticExpression().getFirst();
        ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
        indexExpression.accept(arithmeticExpressionVisitor);
        CobolExpression evaluatedIndex = arithmeticExpressionVisitor.getExpression().evaluate(data);
        int tableIndex = (int) evaluatedIndex.evalAsNumber(data);
        return reference.index(tableIndex);
    }

    public CobolReference getReference(PrimitiveCobolExpression value) {
        return new PrimitiveReference(value.data());
    }
}
