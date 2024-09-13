package org.smojol.toolkit.flowchart;

import org.smojol.common.ast.FlowNode;
import org.smojol.toolkit.ast.PerSectionFlowNodeMermaidVisitor;

import java.util.List;

public class MermainChartBuilder {
    public List<String> build(FlowNode graphRoot) {
        FlowNode rootFlowNode = graphRoot;
        PerSectionFlowNodeMermaidVisitor chartVisitor = new PerSectionFlowNodeMermaidVisitor(graphRoot);
//        FullProgramFlowNodeMermaidVisitor chartVisitor = new FullProgramFlowNodeMermaidVisitor();
        rootFlowNode.accept(chartVisitor, 1);
        return chartVisitor.getLines();
    }
}
