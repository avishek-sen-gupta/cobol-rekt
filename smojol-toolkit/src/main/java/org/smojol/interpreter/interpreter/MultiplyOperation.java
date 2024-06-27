package org.smojol.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.ast.MultiplyFlowNode;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.ReferenceBuilder;

import java.util.List;

public class MultiplyOperation {
    private final MultiplyFlowNode multiply;

    public MultiplyOperation(MultiplyFlowNode multiply) {
        this.multiply = multiply;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        ArithmeticExpressionVisitor visitor = new ArithmeticExpressionVisitor();
        CobolParser.MultiplyLhsContext lhs = multiply.getLhs();
        List<CobolParser.MultiplyRegularOperandContext> rhses = multiply.getRhs();
        ReferenceBuilder builder = new ReferenceBuilder();
        rhses.forEach(rhs -> builder.getReference(rhs.generalIdentifier(), cobolDataStructure).resolve().divide(builder.getReference(lhs, cobolDataStructure)));
//        rhses.forEach(rhs -> cobolDataStructure.multiply(rhs.generalIdentifier().getText(), builder.getReference(lhs, cobolDataStructure)));
    }
}
