package org.smojol.toolkit.ast;

import guru.nidi.graphviz.engine.Engine;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.GraphvizCmdLineEngine;
import guru.nidi.graphviz.model.Factory;
import guru.nidi.graphviz.model.MutableGraph;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeVisitor;
import org.smojol.common.ast.VisitContext;
import org.smojol.common.flowchart.ChartOverlay;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.flowchart.GraphGenerator;
import org.smojol.toolkit.analysis.task.analysis.BuildFlowchartMarkupTask;

import java.io.File;
import java.io.IOException;
import java.util.function.Function;
import java.util.logging.Logger;

public class FlowchartBuilder {
    private final FlowNode root;
    java.util.logging.Logger LOGGER = Logger.getLogger(FlowchartBuilder.class.getName());
    private final MutableGraph graph;

    public FlowchartBuilder(FlowNode root) {
        this.root = root;
        graph = Factory.mutGraph("example1").setDirected(true).setCluster(true);
        Graphviz.useEngine(new GraphvizCmdLineEngine().timeout(5, java.util.concurrent.TimeUnit.HOURS));
    }

    public void build(String dotFilePath, String imageOutputPath, FlowchartOutputFormat outputFormat) throws IOException, InterruptedException {
        ChartOverlay chartOverlay = BuildFlowchartMarkupTask.buildOverlay(root);
        buildChartGraphic(VisitContext::ALWAYS_VISIT, root, chartOverlay);
        write(dotFilePath);
        new GraphGenerator(outputFormat).generateImage(dotFilePath, imageOutputPath);
    }

    private void buildChartGraphic(Function<VisitContext, Boolean> stopCondition, FlowNode root, ChartOverlay chartOverlay) {
        FlowNodeVisitor chartVisitor = new FlowNodeGraphvizVisitor(graph, chartOverlay, stopCondition);
        root.accept(chartVisitor, 1);
    }

    private void write(String dotFilePath) {
        try {
            Graphviz.fromGraph(graph).engine(Engine.DOT)
                    .render(Format.DOT)
                    .toFile(new File(dotFilePath));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
