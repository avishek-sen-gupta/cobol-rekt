package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.ast.MultiplyFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

public class MultiplyOperation implements CobolOperation {
    private final MultiplyFlowNode multiply;

    public MultiplyOperation(MultiplyFlowNode multiply) {
        this.multiply = multiply;
    }

    public void run(CobolDataStructure cobolDataStructure) {
//        CobolParser.MultiplyLhsContext lhs = multiply.getLhs();
//        List<CobolParser.MultiplyRegularOperandContext> rhses = multiply.getRhses();
        DeepReferenceBuilder builder = new DeepReferenceBuilder();
//        rhses.forEach(rhs -> builder.getReference(rhs.generalIdentifier(), cobolDataStructure).resolve().divide(builder.getReference(lhs, cobolDataStructure)));
//        rhses.forEach(rhs -> cobolDataStructure.multiply(rhs.generalIdentifier().getText(), builder.getReference(lhs, cobolDataStructure)));
        CobolExpression sourceProduct = multiply.getSourceExpressions().stream().map(srcExpr -> srcExpr.evaluate(cobolDataStructure)).reduce(new PrimitiveCobolExpression(TypedRecord.typedNumber(1.0)), (product, currentExpr) -> product.multiply(currentExpr, cobolDataStructure));
        multiply.getDestinationExpressions().forEach(to -> builder.getReference(to, cobolDataStructure).set(builder.getReference(sourceProduct, cobolDataStructure)));
    }
}
