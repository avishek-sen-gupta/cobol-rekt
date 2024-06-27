package org.smojol.common.vm.expression;

import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;

public class SimpleConditionExpression extends CobolExpression {
    @Getter private final CobolExpression lhs;
    @Getter private final RelationExpression comparison;
    @Getter private boolean isStandalone = false;

    public SimpleConditionExpression(CobolExpression lhs, CobolExpression comparison) {
        this.lhs = lhs;
        this.comparison = (RelationExpression) comparison;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        if (comparison != null) return comparison.evaluate(lhs, data);
        System.out.println("Comparison clause not present. Will check for level 88 condition...");

        // Level 88 variable condition
        return PrimitiveCobolExpression.primitive(lhs.evaluate(data));
    }

    public CobolExpression standalone() {
        this.isStandalone = true;
        return this;
    }
}
