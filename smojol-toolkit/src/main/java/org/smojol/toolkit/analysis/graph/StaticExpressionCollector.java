package org.smojol.toolkit.analysis.graph;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.CobolExpressionChildVisitor;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.expression.VariableExpression;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

public class StaticExpressionCollector implements CobolExpressionChildVisitor {
    private final CobolDataStructure data;
    List<CobolDataStructure> structures = new ArrayList<>();

    public StaticExpressionCollector(CobolDataStructure data) {
        this.data = data;
    }

    @Override
    public void visit(CobolExpression expression) {
        DeepReferenceBuilder referenceBuilder = new DeepReferenceBuilder();
        if (expression.getClass() == PrimitiveCobolExpression.class) {
            CobolReference reference = referenceBuilder.getReference((PrimitiveCobolExpression) expression);
            structures.add(reference.resolve());
        } else if (expression.getClass() == VariableExpression.class) {
            CobolReference reference = referenceBuilder.getShallowReference(((VariableExpression) expression).getQualifiedDataNameContext(), data);

            // TODO: Dirty hack to ignore indexed elements
            if (reference.resolve().name().contains("$")) return;
            structures.add(reference.resolve());
        }
    }

    public List<CobolDataStructure> structures() {
        return structures;
    }
}
