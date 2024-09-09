package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.logging.Logger;

public class NullCobolExpression extends CobolExpression {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(NullCobolExpression.class.getName());
    private final String EMPTY_STRING = "";
    private final String referenceID;

    public NullCobolExpression(String referenceID) {
        super(ImmutableList.of(), "NULL");
        this.referenceID = referenceID;
    }

    @Override
    public CobolExpression equalTo(CobolExpression other, CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(TypedRecord.FALSE);
    }

    @Override
    public CobolExpression lessThan(CobolExpression other, CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(TypedRecord.FALSE);
    }

    @Override
    public CobolExpression greaterThan(CobolExpression other, CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(TypedRecord.FALSE);
    }

    @Override
    public CobolExpression lessThanOrEqualTo(CobolExpression other, CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(TypedRecord.FALSE);
    }

    @Override
    public CobolExpression greaterThanOrEqualTo(CobolExpression other, CobolDataStructure dataStructures) {
        return new PrimitiveCobolExpression(TypedRecord.FALSE);
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return withWarning2(this, "WARNING: Evaluating null cobol expression", referenceID);
    }

    @Override
    public String description() {
        return operationMnemonic + "()";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.NULL;
    }

    @Override
    public double evalAsNumber(CobolDataStructure data) {
        return withWarning2(Double.NaN, "WARNING: Evaluating null cobol expression as Number, will return NaN", referenceID);
    }

    @Override
    public boolean evalAsBoolean(CobolDataStructure data) {
        return withWarning2(false, "WARNING: Evaluating null cobol expression as Boolean, will return FALSE", referenceID);
    }

    @Override
    public String evalAsString(CobolDataStructure data) {
        return withWarning2(EMPTY_STRING, "WARNING: Evaluating null cobol expression as String, will return '[NULL]'", referenceID);
    }

    @Override
    public String toString() {
        return withWarning2(EMPTY_STRING, "WARNING: Evaluating null cobol expression as toString(), will return '[NULL]'", referenceID);
    }

    @Override
    public CobolExpression add(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: Add Operation requested on null cobol expression", referenceID);
    }

    @Override
    public CobolExpression and(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: AND Operation requested on null cobol expression", referenceID);
    }

    @Override
    public CobolExpression or(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: OR Operation requested on null cobol expression", referenceID);
    }

    @Override
    public CobolExpression subtract(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: Subtract Operation requested on null cobol expression", referenceID);
    }

    @Override
    public CobolExpression divide(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: Divide Operation requested on null cobol expression", referenceID);
    }

    @Override
    public CobolExpression multiply(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: Multiply Operation requested on null cobol expression", referenceID);
    }

    @Override
    protected CobolExpression exponent(CobolExpression other, CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: Exponentiation Operation requested on null cobol expression", referenceID);
    }

    @Override
    protected CobolExpression negative(CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: Negation Operation requested on null cobol expression", referenceID);
    }

    @Override
    protected CobolExpression not(CobolDataStructure dataStructures) {
        return withWarning2(this, "WARNING: NOT Operation requested on null cobol expression", referenceID);
    }

    private static <T> T withWarning2(T v, String message, String referenceID) {
        LOGGER.fine(ConsoleColors.red(message + ". Original Reference ID: " + referenceID));
        return v;
    }
}
