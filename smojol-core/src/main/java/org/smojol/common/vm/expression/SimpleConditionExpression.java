package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

import java.util.logging.Logger;

public class SimpleConditionExpression extends CobolExpression {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(SimpleConditionExpression.class.getName());
    @Getter private final CobolExpression lhs;
    @Getter private final RelationExpression comparison;
    @Getter private boolean isStandalone = false;

    public SimpleConditionExpression(CobolExpression lhs, CobolExpression comparison) {
        super(ImmutableList.of(lhs, comparison), "SIMPLE_COND");
        this.lhs = lhs;
        this.comparison = (RelationExpression) comparison;
    }

    public SimpleConditionExpression(CobolExpression arithmeticExpression) {
        super(ImmutableList.of(), "SIMPLE");
        this.lhs = arithmeticExpression;
        this.comparison = null;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        if (comparison != null) return comparison.evaluate(lhs, data);
        LOGGER.warning("Comparison clause not present. Will check for level 88 condition...");

        // Level 88 variable condition
        return PrimitiveCobolExpression.primitive(lhs.evaluate(data));
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + lhs.description() + ", " + comparison.description() + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.BOOLEAN;
    }

    public CobolExpression standalone() {
        this.isStandalone = true;
        return this;
    }
}
