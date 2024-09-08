package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.ast.MultiplyFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.CobolReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

public class MultiplyOperation implements CobolOperation {
    private final MultiplyFlowNode multiply;

    public MultiplyOperation(MultiplyFlowNode multiply) {
        this.multiply = multiply;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        CobolReferenceBuilder builder = new CobolReferenceBuilder();
        CobolExpression sourceProduct = multiply.getSourceExpressions().stream().map(srcExpr -> srcExpr.evaluate(cobolDataStructure)).reduce(new PrimitiveCobolExpression(TypedRecord.typedNumber(1.0)), (product, currentExpr) -> product.multiply(currentExpr, cobolDataStructure));
        multiply.getDestinationExpressions().forEach(to -> builder.getReference(to, cobolDataStructure).set(builder.getReference(sourceProduct, cobolDataStructure)));
    }
}
