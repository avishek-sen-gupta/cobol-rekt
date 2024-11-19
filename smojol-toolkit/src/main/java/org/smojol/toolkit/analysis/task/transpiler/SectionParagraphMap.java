package org.smojol.toolkit.analysis.task.transpiler;

import lombok.Getter;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.toolkit.ast.ParagraphFlowNode;
import org.smojol.toolkit.ast.SectionFlowNode;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Getter
public class SectionParagraphMap {
    private final Map<SectionFlowNode, List<ParagraphFlowNode>> sectionParagraphsMap;
    private final Map<ParagraphFlowNode, SectionFlowNode> paragraphToSectionMap;
    private final List<SectionFlowNode> sectionFlowNodes;

    public SectionParagraphMap(FlowNode flowRoot) {
        sectionFlowNodes = new FlowNodeNavigator(flowRoot).findAllByType(SectionFlowNode.class);
        sectionParagraphsMap = sectionFlowNodes.stream()
                .map(sfn -> (Pair<SectionFlowNode, List<ParagraphFlowNode>>) ImmutablePair.of(sfn, new FlowNodeNavigator(sfn).findAllByType(ParagraphFlowNode.class)))
                .collect(Collectors.toMap(Pair::getLeft, Pair::getRight));
        paragraphToSectionMap = sectionFlowNodes.stream().flatMap(sfn ->
                new FlowNodeNavigator(sfn).findAllByCondition(n -> n.type() == FlowNodeType.PARAGRAPH).stream()
                        .map(para -> ImmutablePair.of((ParagraphFlowNode) para, sfn))
        ).collect(Collectors.toMap(Pair::getLeft, Pair::getRight));

    }

    public Optional<SectionFlowNode> nextSection(SectionFlowNode n) {
        Optional<SectionFlowNode> flowNode = sectionFlowNodes.stream().filter(sn -> sn == n).findFirst();
        if (flowNode.isEmpty()) return flowNode;
        int nodeIndex = sectionFlowNodes.indexOf(flowNode.get());
        return nodeIndex == sectionFlowNodes.size() - 1 ? Optional.empty() : Optional.of(sectionFlowNodes.get(nodeIndex + 1));
    }

    public Optional<ParagraphFlowNode> nextParagraph(ParagraphFlowNode n) {
        SectionFlowNode sectionFlowNode = paragraphToSectionMap.get(n);
        List<ParagraphFlowNode> parasForSection = sectionParagraphsMap.get(sectionFlowNode);
        int paraIndex = parasForSection.indexOf(n);
        if (paraIndex == -1) return Optional.empty();
        return paraIndex == parasForSection.size() - 1 ? Optional.empty() : Optional.of(parasForSection.get(paraIndex + 1));
    }
}
