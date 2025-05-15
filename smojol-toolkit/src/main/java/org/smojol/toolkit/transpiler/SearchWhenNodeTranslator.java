package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.transpiler.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.SearchFlowNode;
import org.smojol.toolkit.ast.SearchWhenFlowNode;
import org.smojol.toolkit.intermediate.SectionParagraphMap;

import java.util.List;

import static com.mojo.algorithms.list.CarCdr.head;
import static com.mojo.algorithms.list.CarCdr.tail;


public class SearchWhenNodeTranslator {
    public static TranspilerNode build(SearchFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        List<SearchWhenFlowNode> phrases = n.getWhenPhrases().stream().map(node -> (SearchWhenFlowNode) node).toList();
        return new ListIterationTranspilerNode(nodeBuilder.build(n.getSearchExpression()), build(head(phrases).get(), tail(phrases), dataStructures, sectionParagraphMap));
    }

    private static TranspilerNode build(SearchWhenFlowNode n, List<SearchWhenFlowNode> remaining, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode transpilerCondition = nodeBuilder.build(n.getConditionExpression());
        TranspilerCodeBlockNode whenBlock = new DetachedTranspilerCodeBlockNode(n.getWhenFlowNodes().stream().map(node -> TranspilerTreeBuilder.flowToTranspiler(node, dataStructures, sectionParagraphMap)).toList());
//        whenBlock.add(new BreakTranspilerNode());
        whenBlock.add(new JumpTranspilerNode(new ExitIterationScopeLocationNode()));
        if (head(remaining).isEmpty()) return new IfTranspilerNode(transpilerCondition, whenBlock);
        return new IfTranspilerNode(transpilerCondition, whenBlock, new DetachedTranspilerCodeBlockNode(build(head(remaining).get(), tail(remaining), dataStructures, sectionParagraphMap)));
    }
}
