package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.ExpandedEvaluation;
import org.smojol.common.vm.expression.TestActionPair;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.EvaluateFlowNode;

import java.util.List;
import java.util.Optional;

import static org.smojol.common.list.ConsCar.head;
import static org.smojol.common.list.ConsCar.tail;

public class EvaluateNodeTranslator {
    public static TranspilerNode build(EvaluateFlowNode n, CobolDataStructure dataStructures) {
        ExpandedEvaluation deconstructedRepresentation = n.getDeconstructedRepresentation();
        List<TestActionPair> clauses = deconstructedRepresentation.testActionPairs();
        TranspilerCodeBlock elseBody = new TranspilerCodeBlock(deconstructedRepresentation.elseBody().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures)).toList());
        return recursiveOr(head(clauses), tail(clauses), dataStructures, elseBody);
    }

    private static TranspilerNode recursiveOr(Optional<TestActionPair> current, List<TestActionPair> remaining, CobolDataStructure dataStructures, TranspilerCodeBlock elseBody) {
        TranspilerExpressionBuilder builder = new TranspilerExpressionBuilder(dataStructures);
        CobolExpression test = current.get().test();
        List<FlowNode> bodyStatements = current.get().actions();
        TranspilerCodeBlock transpilerBody = new TranspilerCodeBlock(bodyStatements.stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures)).toList());
        if (remaining.isEmpty()) return new IfTranspilerNode(builder.build(test), transpilerBody, elseBody);
        return new IfTranspilerNode(builder.build(test), transpilerBody, recursiveOr(head(remaining), tail(remaining), dataStructures, elseBody));
    }
}
