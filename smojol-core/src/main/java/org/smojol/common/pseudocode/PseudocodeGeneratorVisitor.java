package org.smojol.common.pseudocode;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.IdProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class PseudocodeGeneratorVisitor extends AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> {
    private final IdProvider uuidProvider;
    List<PseudocodeInstruction> instructions = new ArrayList<>();

    public PseudocodeGeneratorVisitor(FlowNode ancestor, IdProvider uuidProvider) {
        super(ancestor);
        this.uuidProvider = uuidProvider;
    }

    @Override
    public void visit(FlowNode node) {
        instructions.addAll(PseudocodeInstructionGenerator.visiting(node, uuidProvider));
    }

    @Override
    public void enter(FlowNode node) {
        instructions.add(PseudocodeInstructionGenerator.entering(node, uuidProvider));
    }

    @Override
    public void exit(FlowNode node) {
        instructions.add(PseudocodeInstructionGenerator.exiting(node, uuidProvider));
    }

    @Override
    public AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> scope(FlowNode n) {
        return new PseudocodeGeneratorVisitor(n, uuidProvider);
    }

    @Override
    public void processChildResults(List<List<PseudocodeInstruction>> childResults) {
        List<PseudocodeInstruction> childInstructions = childResults.stream().flatMap(Collection::stream).toList();
        instructions.addAll(childInstructions);
    }

    @Override
    public List<PseudocodeInstruction> result() {
        return instructions;
    }
}
