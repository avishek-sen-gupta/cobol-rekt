package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.toolkit.ast.MoveFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

import java.util.logging.Logger;

public class MoveOperation implements CobolOperation {
    java.util.logging.Logger LOGGER = Logger.getLogger(MoveOperation.class.getName());
    private final MoveFlowNode move;

    public MoveOperation(MoveFlowNode move) {
        this.move = move;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        LOGGER.finer("From is " + move.getFrom().getText());
        DeepReferenceBuilder referenceBuilder = new DeepReferenceBuilder();
        CobolReference fromReference = referenceBuilder.getReference(move.getFrom(), cobolDataStructure);

//        move.getTos().forEach(to -> cobolDataStructure.set(to.getText(), fromReference));
        move.getTos().forEach(to -> referenceBuilder.getReference(to, cobolDataStructure).resolve().set(fromReference));
    }
}
