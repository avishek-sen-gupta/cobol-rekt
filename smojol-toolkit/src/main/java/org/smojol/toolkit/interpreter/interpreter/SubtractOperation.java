package org.smojol.toolkit.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.toolkit.ast.SubtractFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

import java.util.List;

public class SubtractOperation implements CobolOperation {
    private final SubtractFlowNode subtract;

    public SubtractOperation(SubtractFlowNode subtract) {
        this.subtract = subtract;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        List<CobolParser.SubtractMinuendContext> lhses = subtract.getLhs();
        List<CobolParser.SubtractSubtrahendContext> rhses = subtract.getRhs();
        DeepReferenceBuilder builder = new DeepReferenceBuilder();
        lhses.forEach(lhs -> rhses.forEach(rhs -> builder.getReference(lhs, cobolDataStructure).resolve().subtract(builder.getReference(rhs, cobolDataStructure))));
//        lhses.forEach(lhs -> rhses.forEach(rhs -> cobolDataStructure.subtract(lhs.generalIdentifier().getText(), builder.getReference(rhs, cobolDataStructure))));
    }
}
