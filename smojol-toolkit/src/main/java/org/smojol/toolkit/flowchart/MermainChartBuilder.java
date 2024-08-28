package org.smojol.toolkit.flowchart;

import org.smojol.common.ast.FlowNode;
import org.smojol.toolkit.ast.PerSectionFlowNodeMermaidVisitor;

import java.util.ArrayList;

public class MermainChartBuilder {
    public ArrayList<String> build(FlowNode graphRoot) {
        FlowNode rootFlowNode = graphRoot;
        PerSectionFlowNodeMermaidVisitor chartVisitor = new PerSectionFlowNodeMermaidVisitor(graphRoot);
        rootFlowNode.accept(chartVisitor, 1);
        return chartVisitor.getLines();
    }
}
