package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.expression.ConditionTestTime;
import org.smojol.common.vm.expression.FlowIteration;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ConditionalStatementFlowNode;
import org.smojol.toolkit.ast.PerformInlineFlowNode;
import org.smojol.toolkit.ast.PerformProcedureFlowNode;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

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

    public static TranspilerNode decompose(TranspilerLoop loop) {
        List<TranspilerNode> nodes = new ArrayList<>();
        if (!(loop.getLoopVariable() instanceof NullTranspilerNode)) {
            nodes.add(new SetTranspilerNode(loop.getInitialValue(), loop.getLoopVariable()));
            TranspilerNode body = loop.getBody();
            if (loop.getConditionTestTime() == ConditionTestTime.BEFORE) {
                LabelledTranspilerCodeBlockNode ifNode = new LabelledTranspilerCodeBlockNode(
                        UUID.randomUUID().toString(),
                        ImmutableList.of(new IfTranspilerNode(loop.getTerminateCondition(), new DetachedTranspilerCodeBlockNode(), new DetachedTranspilerCodeBlockNode(new JumpTranspilerNode(new IdLocationNode(body, CodeSentinelType.ENTER))))),
                        ImmutableMap.of());
                nodes.add(ifNode);
                SetTranspilerNode updateNode = new SetTranspilerNode(new ValueOfNode(new AddNode(new ValueOfNode(loop.getLoopVariable()), new ValueOfNode(loop.getLoopUpdate()))),
                        loop.getLoopVariable());
                nodes.add(body);
                nodes.add(updateNode);
                nodes.add(new JumpTranspilerNode(new NamedLocationNode(ifNode.getName())));
                return new TranspilerCodeBlockNode(nodes);
            }
            nodes.add(body);
            SetTranspilerNode updateNode = new SetTranspilerNode(new ValueOfNode(new AddNode(new ValueOfNode(loop.getLoopVariable()), new ValueOfNode(loop.getLoopUpdate()))),
                    loop.getLoopVariable());
            nodes.add(updateNode);
            TranspilerNode ifNode = new IfTranspilerNode(loop.getTerminateCondition(), new DetachedTranspilerCodeBlockNode(), new DetachedTranspilerCodeBlockNode(new JumpTranspilerNode(new IdLocationNode(body, CodeSentinelType.ENTER))));
            nodes.add(ifNode);
            return new TranspilerCodeBlockNode(nodes);
        }
        return loop;
    }

    public static TranspilerNode build(PerformInlineFlowNode n, CobolDataStructure dataStructures) {
        List<FlowIteration> nestedLoops = n.getNestedLoops();
        if (nestedLoops.isEmpty()) return body(n, dataStructures);
        return recurse(nestedLoops, body(n, dataStructures), dataStructures);
    }

    private static TranspilerNode body(FlowNode node, CobolDataStructure dataStructures) {
        List<FlowNode> inlineStatements = node.astChildren().stream().filter(n -> n instanceof ConditionalStatementFlowNode).toList();
        List<TranspilerNode> inlineTranspilerNodes = inlineStatements.stream().map(istmt -> TranspilerTreeBuilder.flowToTranspiler(istmt, dataStructures)).toList();
        return new TranspilerCodeBlockNode(inlineTranspilerNodes);
    }
}
