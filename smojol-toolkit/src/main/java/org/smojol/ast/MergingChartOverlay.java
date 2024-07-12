package org.smojol.ast;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.ChartOverlay;

import java.util.List;

public class MergingChartOverlay implements ChartOverlay {
    private final List<GenericProcessingFlowNode> groups;

    public MergingChartOverlay(List<GenericProcessingFlowNode> groups) {
        this.groups = groups;
    }

    @Override
    public FlowNode block(FlowNode node) {
        List<GenericProcessingFlowNode> containingGroups = groups.stream().filter(g -> g.contains(node)).toList();
        if (containingGroups.isEmpty()) return node;
        if (containingGroups.size() == 1) return containingGroups.getFirst();
        // Filters out lower-level groups which are GenericStatement aggregations
        return containingGroups.stream().filter(g -> !isAtomic(g)).findFirst().get();
    }

    public static boolean isAtomic(FlowNode node) {
        return node.getClass() == GenericStatementFlowNode.class ||
                node.getClass() == MoveFlowNode.class ||
                node.getClass() == DisplayFlowNode.class ||
                node.getClass() == ComputeFlowNode.class ||
                node.getClass() == SubtractFlowNode.class ||
                node.getClass() == MultiplyFlowNode.class ||
                node.getClass() == DivideFlowNode.class ||
                node.getClass() == AddFlowNode.class;
    }
}
