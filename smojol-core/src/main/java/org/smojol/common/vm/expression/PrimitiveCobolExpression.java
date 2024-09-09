package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.function.Function;

public class PrimitiveCobolExpression extends CobolExpression {
    private final TypedRecord value;

    public PrimitiveCobolExpression(TypedRecord value) {
        super(ImmutableList.of(), "PRIMITIVE");
        this.value = value;
    }

    public TypedRecord data() {
        return value;
    }

    public PrimitiveCobolExpression(CobolExpression value) {
        super(ImmutableList.of(), "PRIMITIVE");
        this.value = unwrapped(value);
    }

    private static TypedRecord unwrapped(Object value) {
        Object current = value;
        while (current.getClass() == PrimitiveCobolExpression.class) {
            current = ((PrimitiveCobolExpression) value).data();
        }
        return (TypedRecord) current;
    }

    public static CobolExpression primitive(CobolExpression v) {
        if (v instanceof PrimitiveCobolExpression) return v;
        if (v instanceof NullCobolExpression) return v;
        return new PrimitiveCobolExpression(v);
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return this;
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + value.toString() + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return value.dataType().abstractType();
    }

    @Override
    public double evalAsNumber(CobolDataStructure data) {
        return value.asNumber();
    }

    @Override
    public boolean evalAsBoolean(CobolDataStructure data) {
        return value.asBoolean();
    }

    @Override
    public String evalAsString(CobolDataStructure data) {
        return value.asString();
    }

    @Override
    public CobolExpression equalTo(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.equalTo(o.value)), other);
    }

    @Override
    public CobolExpression lessThan(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.lessThan(o.value)), other);
    }

    @Override
    public CobolExpression greaterThan(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.greaterThan(o.value)), other);
    }

    @Override
    public CobolExpression lessThanOrEqualTo(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.lessThanOrEqualTo(o.value)), other);
    }

    @Override
    public CobolExpression greaterThanOrEqualTo(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.greaterThanOrEqualTo(o.value)), other);
    }

    @Override
    public CobolExpression add(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.add(o.value)), other);
    }

    @Override
    public CobolExpression and(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.and(o.value)), other);
    }

    @Override
    public CobolExpression or(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.or(o.value)), other);
    }

    @Override
    public CobolExpression subtract(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.subtract(o.value)), other);
    }

    @Override
    public CobolExpression divide(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.divide(o.value)), other);
    }

    @Override
    public CobolExpression multiply(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.multiply(o.value)), other);
    }

    @Override
    protected CobolExpression exponent(CobolExpression other, CobolDataStructure dataStructures) {
        return returnIf(o -> new PrimitiveCobolExpression(value.exponent(o.value)), other);
    }

    @Override
    protected CobolExpression negative(CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(value.negative());
    }

    @Override
    protected CobolExpression not(CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(value.not());
    }

    private CobolExpression returnIf(Function<PrimitiveCobolExpression, CobolExpression> evaluation, CobolExpression other) {
        if (!isCompatibleWith(other))
            throw new IncompatibleClassChangeError("Incompatible types");
        PrimitiveCobolExpression otherPrimitiveExpression = (PrimitiveCobolExpression) other;
        return evaluation.apply(otherPrimitiveExpression);
    }

    // TODO: Opportunity to automatically extract or resolve values
    // Evaluate other, and verify that the RESULT is PrimitiveCobolExpression
    // Don't need to be so strict
    private boolean isCompatibleWith(CobolExpression other) {
        return other instanceof PrimitiveCobolExpression;
//        PrimitiveCobolExpression otherPrimitiveExpression = (PrimitiveCobolExpression) other;
//        return value.isCompatibleWith(otherPrimitiveExpression.value);
    }
}
