package org.smojol.common.ast;

import com.mojo.algorithms.id.IdProvider;
import com.mojo.algorithms.navigation.AggregatingTranspilerNodeVisitor;
import com.mojo.algorithms.domain.TranspilerInstruction;
import com.mojo.algorithms.domain.TranspilerInstructionGenerator;
import com.mojo.algorithms.domain.TranspilerNode;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class TranspilerInstructionGeneratorVisitor extends AggregatingTranspilerNodeVisitor<List<TranspilerInstruction>> {
    private final IdProvider uuidProvider;
    List<TranspilerInstruction> instructions = new ArrayList<>();

    public TranspilerInstructionGeneratorVisitor(IdProvider uuidProvider) {
        this.uuidProvider = uuidProvider;
    }

    @Override
    public void visit(TranspilerNode node) {
        instructions.addAll(TranspilerInstructionGenerator.body(node, uuidProvider));
    }

    @Override
    public void enter(TranspilerNode node) {
        instructions.add(TranspilerInstructionGenerator.entering(node, uuidProvider));
    }

    @Override
    public void exit(TranspilerNode node) {
        instructions.add(TranspilerInstructionGenerator.exiting(node, uuidProvider));
    }

    @Override
    public AggregatingTranspilerNodeVisitor<List<TranspilerInstruction>> scope(TranspilerNode n) {
        return new TranspilerInstructionGeneratorVisitor(uuidProvider);
    }

    @Override
    public void processChildResults(List<List<TranspilerInstruction>> childResults) {
        List<TranspilerInstruction> childInstructions = childResults.stream().flatMap(Collection::stream).toList();
        instructions.addAll(childInstructions);
    }

    @Override
    public List<TranspilerInstruction> result() {
        return instructions;
    }
}
