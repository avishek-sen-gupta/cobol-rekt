package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.expression.FlowIteration;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ConditionalStatementFlowNode;
import org.smojol.toolkit.ast.PerformInlineFlowNode;
import org.smojol.toolkit.ast.PerformProcedureFlowNode;

import java.util.List;

public class PerformProcedureNodeBuilder {
    public static TranspilerNode build(PerformProcedureFlowNode n, CobolDataStructure dataStructures) {
        List<FlowIteration> nestedLoops = n.getNestedLoops();
        TranspilerNode body = new JumpTranspilerNode(new NamedLocationNode(n.getStartNode().name()), new NamedLocationNode(n.getEndNode().name()));
        return recurse(nestedLoops, body, dataStructures);
    }

    private static TranspilerNode recurse(List<FlowIteration> loops, TranspilerNode body, CobolDataStructure dataStructures) {
        if (loops.isEmpty()) return body;
        if (loops.size() == 1) return toTranspilerLoop(loops.getFirst(), body, dataStructures);
        return toTranspilerLoop(loops.getFirst(), recurse(loops.subList(1, loops.size()), body, dataStructures), dataStructures);
    }

    private static TranspilerNode toTranspilerLoop(FlowIteration loop, TranspilerNode body, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder expressionBuilder = new TranspilerExpressionBuilder(dataStructures);
        return new TranspilerLoop(expressionBuilder.build(loop.loopVariable()),
                expressionBuilder.build(loop.initialValue()),
                expressionBuilder.build(loop.maxValue()),
                expressionBuilder.build(loop.condition()),
                new TranspilerLoopUpdate(expressionBuilder.build(loop.loopUpdate().updateDelta())),
                loop.conditionTestTime(), body
        );
    }

    public static TranspilerNode build(PerformInlineFlowNode n, CobolDataStructure dataStructures) {
        List<FlowIteration> nestedLoops = n.getNestedLoops();
        if (nestedLoops.isEmpty()) return body(n, dataStructures);
        return recurse(nestedLoops, body(n, dataStructures), dataStructures);
    }

    private static TranspilerNode body(FlowNode node, CobolDataStructure dataStructures) {
        List<FlowNode> inlineStatements = node.astChildren().stream().filter(n -> n instanceof ConditionalStatementFlowNode).toList();
        List<TranspilerNode> inlineTranspilerNodes = inlineStatements.stream().map(istmt -> TranspilerTreeBuilder.flowToTranspiler(istmt, dataStructures)).toList();
        return new TranspilerCodeBlock(inlineTranspilerNodes);
    }
}
