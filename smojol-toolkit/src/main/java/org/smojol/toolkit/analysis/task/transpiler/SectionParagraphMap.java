package org.smojol.toolkit.analysis.task.transpiler;

import lombok.Getter;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.navigation.FlowNodeNavigator;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Getter
public class SectionParagraphMap {
    private final Map<FlowNode, List<FlowNode>> sectionParagraphsPairs;
    private final Map<FlowNode, FlowNode> paragraphToSectionMap;

    public SectionParagraphMap(FlowNode flowRoot) {
        List<FlowNode> sectionFlowNodes = new FlowNodeNavigator(flowRoot).findAllByCondition(fn -> fn.type() == FlowNodeType.SECTION);
        sectionParagraphsPairs = sectionFlowNodes.stream()
                .map(sfn -> (Pair<FlowNode, List<FlowNode>>) ImmutablePair.of(sfn, new FlowNodeNavigator(sfn).findAllByCondition(n -> n.type() == FlowNodeType.PARAGRAPH)))
                .collect(Collectors.toMap(Pair::getLeft, Pair::getRight));
        paragraphToSectionMap = sectionFlowNodes.stream().flatMap(sfn ->
                new FlowNodeNavigator(sfn).findAllByCondition(n -> n.type() == FlowNodeType.PARAGRAPH).stream()
                        .map(para -> ImmutablePair.of(para, sfn))
        ).collect(Collectors.toMap(Pair::getLeft, Pair::getRight));

    }
}
