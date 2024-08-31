package org.smojol.common.vm.expression;

import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParserBaseVisitor;

@Getter
public class AntlrCobolExpressionVisitor extends CobolParserBaseVisitor<CobolExpression> {
    protected CobolExpression expression;

    protected BinaryCobolLogicExpression lastRhsLogicExpression(BinaryCobolLogicExpression current) {
        while (current.getRhs() instanceof BinaryCobolLogicExpression) {
            current = (BinaryCobolLogicExpression) current.getRhs();
        }
        return current;
    }

    protected CobolExpression graft(LogicOperation operator, CobolExpression lhs, CobolExpression rhs) {
        if (lhs.getClass() != AndExpression.class && lhs.getClass() != OrExpression.class) {
            return LogicOperation.create(operator, lhs, rhs);
        } else if (operator == LogicOperation.OR) {
            return LogicOperation.create(operator, lhs, rhs);
        }
        BinaryCobolLogicExpression current = (BinaryCobolLogicExpression) lhs;
        BinaryCobolLogicExpression lastRHS = lastRhsLogicExpression(current);
        lastRHS.setRhs(LogicOperation.create(operator, lastRHS.rhs, rhs));
        return lhs;
    }
}
