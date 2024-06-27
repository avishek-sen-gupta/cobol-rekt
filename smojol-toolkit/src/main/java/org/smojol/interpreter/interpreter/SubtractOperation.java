package org.smojol.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.ast.SubtractFlowNode;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.ReferenceBuilder;

import java.util.List;

public class SubtractOperation {
    private final SubtractFlowNode subtract;

    public SubtractOperation(SubtractFlowNode subtract) {
        this.subtract = subtract;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        ArithmeticExpressionVisitor visitor = new ArithmeticExpressionVisitor();
        List<CobolParser.SubtractMinuendContext> lhses = subtract.getLhs();
        List<CobolParser.SubtractSubtrahendContext> rhses = subtract.getRhs();
        ReferenceBuilder builder = new ReferenceBuilder();
        lhses.forEach(lhs -> rhses.forEach(rhs -> builder.getReference(lhs, cobolDataStructure).resolve().subtract(builder.getReference(rhs, cobolDataStructure))));
//        lhses.forEach(lhs -> rhses.forEach(rhs -> cobolDataStructure.subtract(lhs.generalIdentifier().getText(), builder.getReference(rhs, cobolDataStructure))));
    }
}
