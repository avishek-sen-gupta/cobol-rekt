package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.DeepReferenceBuilder;

public class VariableExpression extends CobolExpression {
    @Getter private final CobolParser.VariableUsageNameContext usageName;

    public VariableExpression(CobolParser.VariableUsageNameContext usageName) {
        super(ImmutableList.of());
        this.usageName = usageName;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        CobolDataStructure structure = data.reference(usageName.getText());
        TypedRecord value = structure.getValue();
        return value == TypedRecord.NULL ? new NullCobolExpression(usageName.getText()) : new PrimitiveCobolExpression(value);
    }

    @Override
    public CobolDataStructure reference(CobolDataStructure data) {
        CobolReference ref = new DeepReferenceBuilder().getReference(usageName, data);
        return ref.resolve();
    }

    public String name() {
        return usageName.getText();
    }
}
