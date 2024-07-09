package org.smojol.interpreter.interpreter;

import org.smojol.ast.MoveFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.DeepReferenceBuilder;

public class MoveOperation {
    private final MoveFlowNode move;

    public MoveOperation(MoveFlowNode move) {
        this.move = move;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        System.out.println("From is " + move.getFrom().getText());
        DeepReferenceBuilder referenceBuilder = new DeepReferenceBuilder();
        CobolReference fromReference = referenceBuilder.getReference(move.getFrom(), cobolDataStructure);

//        move.getTos().forEach(to -> cobolDataStructure.set(to.getText(), fromReference));
        move.getTos().forEach(to -> referenceBuilder.getReference(to, cobolDataStructure).resolve().set(fromReference));
    }
}
