package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.transpiler.DetachedTranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.IfTranspilerNode;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.TranspilerNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.ExpandedEvaluation;
import org.smojol.common.vm.expression.TestActionPair;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.EvaluateFlowNode;
import org.smojol.toolkit.intermediate.SectionParagraphMap;

import java.util.List;
import java.util.Optional;

import static com.mojo.algorithms.list.CarCdr.head;
import static com.mojo.algorithms.list.CarCdr.tail;


public class EvaluateNodeTranslator {
    public static TranspilerNode build(EvaluateFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        ExpandedEvaluation deconstructedRepresentation = n.getDeconstructedRepresentation();
        List<TestActionPair> clauses = deconstructedRepresentation.testActionPairs();
        TranspilerCodeBlockNode elseBody = new DetachedTranspilerCodeBlockNode(deconstructedRepresentation.elseBody().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures, sectionParagraphMap)).toList());
        return recursiveOr(head(clauses), tail(clauses), dataStructures, elseBody, sectionParagraphMap);
    }

    private static TranspilerNode recursiveOr(Optional<TestActionPair> current, List<TestActionPair> remaining, CobolDataStructure dataStructures, TranspilerCodeBlockNode elseBody, SectionParagraphMap sectionParagraphMap) {
        TranspilerExpressionBuilder builder = new TranspilerExpressionBuilder(dataStructures);
        CobolExpression test = current.get().test();
        List<FlowNode> bodyStatements = current.get().actions();
        TranspilerCodeBlockNode transpilerBody = new DetachedTranspilerCodeBlockNode(bodyStatements.stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures, sectionParagraphMap)).toList());
        if (remaining.isEmpty()) return new IfTranspilerNode(builder.build(test), transpilerBody, elseBody);
        return new IfTranspilerNode(builder.build(test), transpilerBody, recursiveOr(head(remaining), tail(remaining), dataStructures, elseBody, sectionParagraphMap));
    }
}
