package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.exception.UnsupportedClassConditionException;

public class ClassConditionBuilder {
    public CobolExpression build(CobolParser.FixedComparisonContext fixedComparisonContext, CobolExpression expression) {
        ClassConditionExpression classConditionExpression = condition(fixedComparisonContext, expression);
        return fixedComparisonContext.NOT() != null ? new NotExpression(classConditionExpression) : classConditionExpression;
    }

    public ClassConditionExpression condition(CobolParser.FixedComparisonContext fixedComparisonContext, CobolExpression expression) {
        if (fixedComparisonContext.NUMERIC() != null) return IsNumericCondition.isNumeric(expression);
        else if (fixedComparisonContext.ALPHABETIC() != null) return IsAlphabeticCondition.isAlphabetic(expression);
        else if (fixedComparisonContext.ALPHABETIC_LOWER() != null) return IsAlphabeticCondition.isLowercase(expression);
        else if (fixedComparisonContext.ALPHABETIC_UPPER() != null) return IsAlphabeticCondition.isUppercase(expression);
        else if (fixedComparisonContext.POSITIVE() != null) return IsNumericCondition.isPositive(expression);
        else if (fixedComparisonContext.NEGATIVE() != null) return IsNumericCondition.isNegative(expression);
        else if (fixedComparisonContext.ZERO() != null) return IsNumericCondition.isZero(expression);
        else throw new UnsupportedClassConditionException(fixedComparisonContext.getText());
    }
}
