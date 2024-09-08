package org.smojol.common.vm.expression;

import org.apache.commons.lang3.math.NumberUtils;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

import java.util.function.Function;

public class IsNumericCondition extends ClassConditionExpression {
    private final Function<Double, Boolean> matcher;

    public IsNumericCondition(CobolExpression expression, Function<Double, Boolean> matcher) {
        super(expression, "IS_NUMERIC");
        this.matcher = matcher;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return new PrimitiveCobolExpression(matches((PrimitiveCobolExpression) expression.evaluate(data)));
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + expression.description() + ")";
    }

    private TypedRecord matches(PrimitiveCobolExpression expression) {
        String s = expression.data().value().toString();
        if (!NumberUtils.isParsable(s)) return TypedRecord.FALSE;
        return TypedRecord.typedBoolean(matcher.apply(Double.parseDouble(s)));
    }

    public static ClassConditionExpression isNumeric(CobolExpression expression) {
        return new IsNumericCondition(expression, x -> true);
    }

    public static ClassConditionExpression isPositive(CobolExpression expression) {
        return new IsNumericCondition(expression, x -> x >= 0);
    }

    public static ClassConditionExpression isNegative(CobolExpression expression) {
        return new IsNumericCondition(expression, x -> x < 0);
    }

    public static ClassConditionExpression isZero(CobolExpression expression) {
        return new IsNumericCondition(expression, x -> x == 0);
    }
}
