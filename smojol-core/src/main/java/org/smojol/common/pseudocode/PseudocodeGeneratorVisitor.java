package org.smojol.common.pseudocode;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.FlowNode;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class PseudocodeGeneratorVisitor extends AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> {
    List<PseudocodeInstruction> instructions = new ArrayList<>();

    public PseudocodeGeneratorVisitor(FlowNode ancestor) {
        super(ancestor);
    }

    @Override
    public void visit(FlowNode node) {
        instructions.add(PseudocodeInstructionGenerator.visiting(node));
    }

    @Override
    public void enter(FlowNode node) {
        instructions.add(PseudocodeInstructionGenerator.entering(node));
    }

    @Override
    public void exit(FlowNode node) {
        instructions.add(PseudocodeInstructionGenerator.exiting(node));
    }

    @Override
    public AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> scope(FlowNode n) {
        return new PseudocodeGeneratorVisitor(n);
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