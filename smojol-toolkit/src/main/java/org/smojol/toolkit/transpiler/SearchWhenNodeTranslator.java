package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.SearchFlowNode;
import org.smojol.toolkit.ast.SearchWhenFlowNode;

import java.util.List;

import static org.smojol.common.list.ConsCar.head;
import static org.smojol.common.list.ConsCar.tail;

public class SearchWhenNodeTranslator {
    public static TranspilerNode build(SearchFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        List<SearchWhenFlowNode> phrases = n.getWhenPhrases().stream().map(node -> (SearchWhenFlowNode) node).toList();
        return new ListIterationTranspilerNode(nodeBuilder.build(n.getSearchExpression()), build(head(phrases).get(), tail(phrases), dataStructures));
    }

    private static TranspilerNode build(SearchWhenFlowNode n, List<SearchWhenFlowNode> remaining, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode transpilerCondition = nodeBuilder.build(n.getConditionExpression());
        TranspilerCodeBlock whenBlock = new TranspilerCodeBlock(n.getWhenFlowNodes().stream().map(node -> TranspilerTreeBuilder.flowToTranspiler(node, dataStructures)).toList());
//        whenBlock.add(new BreakTranspilerNode());
        whenBlock.add(new JumpTranspilerNode(new ExitIterationScopeLocationNode()));
        if (head(remaining).isEmpty()) return new IfTranspilerNode(transpilerCondition, whenBlock);
        return new IfTranspilerNode(transpilerCondition, whenBlock, new TranspilerCodeBlock(build(head(remaining).get(), tail(remaining), dataStructures)));
    }
}
