package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.ReferenceBuilder;

public class VariableExpression extends CobolExpression {
    @Getter private final CobolParser.QualifiedDataNameContext qualifiedDataNameContext;

    public VariableExpression(CobolParser.QualifiedDataNameContext qualifiedDataNameContext) {
        super(ImmutableList.of());
        this.qualifiedDataNameContext = qualifiedDataNameContext;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        CobolReference ref = new ReferenceBuilder().getReference(qualifiedDataNameContext, data);
        TypedRecord value = ref.resolve().getValue();
        return value == TypedRecord.NULL ? new NullCobolExpression(qualifiedDataNameContext.getText()) : new PrimitiveCobolExpression(value);
    }
}
