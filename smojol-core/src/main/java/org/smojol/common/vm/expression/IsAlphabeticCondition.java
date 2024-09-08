package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

import java.util.function.Function;

public class IsAlphabeticCondition extends ClassConditionExpression {
    private final Function<Integer, Boolean> matcher;

    public IsAlphabeticCondition(CobolExpression expression, Function<Integer, Boolean> matcher) {
        super(expression, "IS_ALPHABETIC");
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
        Boolean isAlphabetic = s.chars().mapToObj(c -> (char) c).map(i -> matcher.apply((int) i)).reduce(true, (a, b) -> a && b);
        return TypedRecord.typedBoolean(isAlphabetic.booleanValue());
    }

    public static ClassConditionExpression isAlphabetic(CobolExpression e) {
        return new IsAlphabeticCondition(e, Character::isAlphabetic);
    }

    public static ClassConditionExpression isLowercase(CobolExpression e) {
        return new IsAlphabeticCondition(e, Character::isLowerCase);
    }

    public static ClassConditionExpression isUppercase(CobolExpression e) {
        return new IsAlphabeticCondition(e, Character::isUpperCase);
    }
}
