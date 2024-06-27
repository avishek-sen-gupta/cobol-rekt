package org.smojol.interpreter.interpreter;

import org.smojol.ast.ComputeFlowNode;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.ReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;

public class ComputeOperation {
    private final ComputeFlowNode compute;

    public ComputeOperation(ComputeFlowNode compute) {
        this.compute = compute;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        ArithmeticExpressionVisitor visitor = new ArithmeticExpressionVisitor();
        compute.getRhs().accept(visitor);
        PrimitiveCobolExpression value = (PrimitiveCobolExpression) visitor.getExpression().evaluate(cobolDataStructure);
        ReferenceBuilder referenceBuilder = new ReferenceBuilder();
        CobolReference rhs = referenceBuilder.getReference(value);
//        compute.getDestinations().forEach(d -> cobolDataStructure.set(d.generalIdentifier().getText(), new PrimitiveReference(value.data())));
        compute.getDestinations().forEach(d -> referenceBuilder.getReference(d.generalIdentifier(), cobolDataStructure).resolve().set(rhs));
    }

}
