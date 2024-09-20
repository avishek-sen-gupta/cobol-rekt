package org.smojol.toolkit.quad.generators;

import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.ConditionVisitor;
import org.smojol.toolkit.ast.IfFlowNode;

public class IfQuadGeneration extends QuadGeneration {
    public IfQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        IfFlowNode ifFlowNode = instruction.typedNode(IfFlowNode.class);
        ConditionVisitor conditionVisitor = new ConditionVisitor(null);
        ifFlowNode.getCondition().accept(conditionVisitor);
        CobolExpression expression = conditionVisitor.getExpression();
        ExpressionQuadGenerator visitor = new ExpressionQuadGenerator(symbolTable, symbolReferenceBuilder);
        visitor.build(expression);
        QuadSequence quads = visitor.getQuads();
        SymbolReference conditionResultReference = quads.lastResult();
        return new QuadSequence(symbolTable);
    }
}
