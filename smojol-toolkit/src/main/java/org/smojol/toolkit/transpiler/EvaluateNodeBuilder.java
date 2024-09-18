package org.smojol.toolkit.transpiler;

import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.EvaluateFlowNode;

import java.util.List;
import java.util.Optional;

import static org.smojol.common.list.ConsCar.head;
import static org.smojol.common.list.ConsCar.tail;

public class EvaluateNodeBuilder {
    public static TranspilerNode build(EvaluateFlowNode n, CobolDataStructure dataStructures) {
        Pair<List<CobolExpression>, List<Pair<CobolExpression, List<FlowNode>>>> deconstructedRepresentation = n.getDeconstructedRepresentation();
        List<Pair<CobolExpression, List<FlowNode>>> clauses = deconstructedRepresentation.getRight();
        return recursiveOr(head(clauses), tail(clauses), dataStructures);
    }

    private static TranspilerNode recursiveOr(Optional<Pair<CobolExpression, List<FlowNode>>> current, List<Pair<CobolExpression, List<FlowNode>>> remaining, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder builder = new TranspilerExpressionBuilder(dataStructures);
        CobolExpression test = current.get().getLeft();
        List<FlowNode> bodyStatements = current.get().getRight();
        TranspilerCodeBlock transpilerBody = new TranspilerCodeBlock(bodyStatements.stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures)).toList());
        if (remaining.isEmpty()) return new IfTranspilerNode(builder.build(test), transpilerBody);
        return new IfTranspilerNode(builder.build(test), transpilerBody, recursiveOr(head(remaining), tail(remaining), dataStructures));
    }
}
