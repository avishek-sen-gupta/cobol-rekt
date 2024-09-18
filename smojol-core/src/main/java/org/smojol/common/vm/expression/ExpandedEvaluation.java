package org.smojol.common.vm.expression;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public record ExpandedEvaluation(List<CobolExpression> evalSubjects, List<TestActionPair> testActionPairs,
                          List<FlowNode> elseBody) {
    public void buildFlow() {
        elseBody.forEach(FlowNode::buildFlow);
        testActionPairs.forEach(tap -> tap.actions().forEach(FlowNode::buildFlow));
    }

    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        elseBody.forEach(s -> s.resolve(symbolTable, dataStructures));
        testActionPairs.forEach(tap -> tap.actions().forEach(act -> act.resolve(symbolTable, dataStructures)));
    }
}
