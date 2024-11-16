package org.smojol.toolkit.analysis.task.transpiler;

import org.smojol.common.ast.TranspilerInstructionGeneratorVisitor;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.navigation.AggregatingTranspilerNodeTraversal;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public class BuildTranspilerInstructionsFromIntermediateTreeTask {
    private final IncrementingIdProvider idProvider;
    private final TranspilerNode root;

    public BuildTranspilerInstructionsFromIntermediateTreeTask(TranspilerNode root, IncrementingIdProvider idProvider) {
        this.root = root;
        this.idProvider = idProvider;
    }

    public List<TranspilerInstruction> run() {
        TranspilerInstructionGeneratorVisitor visitor = new TranspilerInstructionGeneratorVisitor(idProvider);
        new AggregatingTranspilerNodeTraversal<List<TranspilerInstruction>>().accept(root, visitor);
        return visitor.result();
    }
}
