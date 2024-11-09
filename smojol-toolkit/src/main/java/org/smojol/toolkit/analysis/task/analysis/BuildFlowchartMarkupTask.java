package org.smojol.toolkit.analysis.task.analysis;

import guru.nidi.graphviz.engine.Engine;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Factory;
import guru.nidi.graphviz.model.MutableGraph;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeVisitor;
import org.smojol.common.ast.VisitContext;
import org.smojol.common.flowchart.ChartOverlay;
import org.smojol.toolkit.ast.FlowNodeGraphvizVisitor;
import org.smojol.toolkit.ast.FlowNodeOverlayVisitor;

public class BuildFlowchartMarkupTask {

    private final FlowNode root;

    public BuildFlowchartMarkupTask(FlowNode root) {
        this.root = root;
    }

    public String run() {
        MutableGraph graph = Factory.mutGraph("example1").setDirected(true).setCluster(true);
        FlowNodeVisitor chartVisitor = new FlowNodeGraphvizVisitor(graph, buildOverlay(root), VisitContext::ALWAYS_VISIT);
        root.accept(chartVisitor, 1);
        return Graphviz.fromGraph(graph).engine(Engine.DOT)
                .render(Format.DOT)
                .toString();
    }

    public static ChartOverlay buildOverlay(FlowNode root) {
        FlowNodeOverlayVisitor compressionVisitor = new FlowNodeOverlayVisitor(root);
        root.accept(compressionVisitor, 1);
        compressionVisitor.report();
        return compressionVisitor.overlay();
    }
}
