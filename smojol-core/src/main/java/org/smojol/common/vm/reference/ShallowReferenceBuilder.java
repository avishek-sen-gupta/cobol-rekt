package org.smojol.common.vm.reference;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.LiteralResolver;
import org.smojol.common.vm.type.TypedRecord;

public class ShallowReferenceBuilder {
    private TypedRecord typedValue(CobolParser.MoveToSendingAreaContext from) {
        String v = new LiteralResolver().resolvedLiteral(from.literal());
        return TypedRecord.typedString(v.toString());
    }

    public CobolReference getShallowReference(CobolParser.AddFromContext from, CobolDataStructure dataStructure) {
        if (from.generalIdentifier() != null) return getShallowReference(from.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(from.literal().getText()));
    }

    public CobolReference getShallowReference(CobolParser.SubtractSubtrahendContext rhs, CobolDataStructure dataStructure) {
        if (rhs.generalIdentifier() != null) return getShallowReference(rhs.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(rhs.literal().getText()));
    }

    public CobolReference getShallowReference(CobolParser.MultiplyLhsContext lhs, CobolDataStructure dataStructure) {
        if (lhs.generalIdentifier() != null) return getShallowReference(lhs.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(lhs.literal().getText()));
    }

    public CobolReference getShallowReference(CobolParser.DivisorContext divisor, CobolDataStructure dataStructure) {
        if (divisor.generalIdentifier() != null) return getShallowReference(divisor.generalIdentifier(), dataStructure);
        return new PrimitiveReference(TypedRecord.typedNumber(divisor.literal().getText()));
    }

    public CobolReference getShallowReference(CobolParser.GeneralIdentifierContext to, CobolDataStructure data) {
        if (to.functionCall() != null) return new FunctionCallCobolReference(to.functionCall());
        // TODO: This needs to be a recursive chain if specialRegisters are nested. This is a very simplistic implementation
        else if (to.specialRegister() != null) return new VariableCobolReference(data.reference(to.specialRegister().generalIdentifier().qualifiedDataName().variableUsageName().getText()));
        return new VariableCobolReference(data.reference(to.qualifiedDataName().variableUsageName().getText()));
    }

    public CobolReference getShallowReference(CobolParser.AddToContext to, CobolDataStructure data) {
        return getShallowReference(to.generalIdentifier(), data);
    }

    public CobolReference getShallowReference(CobolParser.SubtractMinuendContext lhs, CobolDataStructure data) {
        return getShallowReference(lhs.generalIdentifier(), data);
    }

    public CobolReference getShallowReference(CobolParser.MoveToSendingAreaContext to, CobolDataStructure dataStructure) {
        if (to.generalIdentifier() != null) return getShallowReference(to.generalIdentifier(), dataStructure);
        return new PrimitiveReference(typedValue(to));
    }
}
