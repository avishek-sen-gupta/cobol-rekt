package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.DeepReferenceBuilder;

public class VariableExpression extends CobolExpression {
    @Getter private final String name;

    public VariableExpression(String name) {
        super(ImmutableList.of(), "VAR");
        this.name = name;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        CobolDataStructure structure = data.reference(name);
        TypedRecord value = structure.getValue();
        return value == TypedRecord.NULL ? new NullCobolExpression(name) : new PrimitiveCobolExpression(value);
    }

    @Override
    public String description() {
        return operationMnemonic + "('" + name + "')";
    }

    @Override
    public CobolDataStructure reference(CobolDataStructure data) {
        CobolReference ref = new DeepReferenceBuilder().getReference(name, data);
        return ref.resolve();
    }
}
