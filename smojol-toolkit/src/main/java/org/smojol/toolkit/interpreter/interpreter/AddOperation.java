package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.ast.AddFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

public class AddOperation implements CobolOperation {
    private final AddFlowNode add;

    public AddOperation(AddFlowNode add) {
        this.add = add;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        DeepReferenceBuilder builder = new DeepReferenceBuilder();
        CobolExpression sourceSum = add.getSourceExpressions().stream().map(srcExpr -> srcExpr.evaluate(cobolDataStructure)).reduce(new PrimitiveCobolExpression(TypedRecord.typedNumber(0)), (sum, currentExpr) -> sum.add(currentExpr, cobolDataStructure));
        add.getDestinationExpressions().forEach(to -> builder.getReference(to, cobolDataStructure).set(builder.getReference(sourceSum, cobolDataStructure)));
    }
}
