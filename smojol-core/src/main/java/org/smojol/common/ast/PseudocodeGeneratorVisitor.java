package org.smojol.common.ast;

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
        instructions.add(new PseudocodeInstruction("DETAIL / " + node.label(), node));
    }

    @Override
    public void enter(FlowNode node) {
        instructions.add(new PseudocodeInstruction("ENTER / " + node.label(), node));
    }

    @Override
    public void exit(FlowNode node) {
        instructions.add(new PseudocodeInstruction("EXIT / " + node.label(), node));
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
