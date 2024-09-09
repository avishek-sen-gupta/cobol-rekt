package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

public class IdmsExpression extends CobolExpression {
    private final ParseTree expression;

    public IdmsExpression(ParseTree expression) {
        super(ImmutableList.of(), "IDMS");
        this.expression = expression;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        // TODO: Keep an eye on this. Harmless, but still...
        return new PrimitiveCobolExpression(TypedRecord.typedNumber(10));
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + expression.getText() + ")";
    }

    // TODO: This is sus
    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.BOOLEAN;
    }
}
