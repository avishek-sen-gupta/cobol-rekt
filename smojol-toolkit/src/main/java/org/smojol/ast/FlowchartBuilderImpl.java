package org.smojol.ast;

import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.*;
import guru.nidi.graphviz.model.Factory;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.model.MutableNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.*;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.stack.CobolStackFrames;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.io.File;
import java.io.IOException;
import java.util.function.Function;

import static guru.nidi.graphviz.model.Factory.mutGraph;
import static guru.nidi.graphviz.model.Factory.mutNode;

public class FlowchartBuilderImpl implements FlowchartBuilder {
    private final FlowNodeService flowNodeService;
    private final CobolDataStructure dataStructures;
    private FlowNode graphRoot;
    private CobolEntityNavigator cobolEntityNavigator;
    private MutableGraph graph;
    private ChartOverlay overlay;

    public FlowchartBuilderImpl(CobolEntityNavigator cobolEntityNavigator, CobolDataStructure dataStructures) {
        this.cobolEntityNavigator = cobolEntityNavigator;
        flowNodeService = new FlowNodeServiceImpl(cobolEntityNavigator, dataStructures, new UUIDProvider());
        this.dataStructures = dataStructures;
        graph = Factory.mutGraph("example1").setDirected(true).setCluster(true);
        Graphviz.useEngine(new GraphvizCmdLineEngine().timeout(5, java.util.concurrent.TimeUnit.HOURS));
    }

    @Override
    public FlowchartBuilder buildDotStructure(Function<VisitContext, Boolean> stopCondition) {
        return buildChartGraphic(stopCondition);
    }

    @Override
    public FlowchartBuilder buildDotStructure() {
        return buildDotStructure(VisitContext::ALWAYS_VISIT);
    }

    @Override
    public FlowchartBuilder buildFlowAST(ParseTree node) {
        FlowNode rootFlowNode = flowNodeService.node(node, null, new CobolStackFrames());
        rootFlowNode.buildFlow();
        graphRoot = rootFlowNode;
        return this;
    }

    private FlowchartBuilder buildChartGraphic(Function<VisitContext, Boolean> stopCondition) {
        FlowNode rootFlowNode = graphRoot;
        FlowNodeVisitor chartVisitor = new FlowNodeGraphvizVisitor(graph, overlay, stopCondition);
        rootFlowNode.accept(chartVisitor, 1);
        return this;
    }

    @Override
    public FlowchartBuilder write(String dotFilePath) {
        try {
            Graphviz.fromGraph(graph).engine(Engine.DOT)
                    .render(Format.DOT)
                    .toFile(new File(dotFilePath));
            return this;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public FlowchartBuilder outline(ParseTree groupRoot, String clusterLabel) {
        NodeCollector collectorVisitor = new NodeCollector(flowNodeService);
        groupRoot.accept(collectorVisitor);
        MutableGraph outliningCluster = mutGraph(clusterLabel).setCluster(true).graphAttrs().add("bgcolor", Color.LIGHTGREY.value);
        collectorVisitor.getCollectedNodes().forEach(n -> outliningCluster.add(mutNode(n.toString())));
        graph.add(outliningCluster);
        return this;
    }

    @Override
    public FlowchartBuilder connectToComment(String explanation, ParseTree symbol) {
        FlowNode explainedNode = flowNodeService.existingNode(symbol);
        if (explainedNode == null) return this;
        System.out.println(String.format("Linking EXPLANATION : %s to %s", explanation, explainedNode));
        MutableNode explanationNode = mutNode(formatted(explanation, 30));
        MutableNode explainedTarget = mutNode(explainedNode.toString());
        graph.add(explanationNode.addLink(explanationNode.linkTo(explainedTarget).with("color", Color.LIGHTGREY.value)));
        return this;
    }

    @Override
    public FlowchartBuilder createComment(String comment) {
        graph.add(commentStyle(mutNode(formatted(comment, 30))));
        return this;
    }

    @Override
    public FlowchartBuilder buildOverlay() {
        FlowNode rootFlowNode = graphRoot;
        FlowNodeOverlayVisitor compressionVisitor = new FlowNodeOverlayVisitor(rootFlowNode);
        rootFlowNode.accept(compressionVisitor, 1);
        compressionVisitor.report();
        overlay = compressionVisitor.overlay();
        return this;
    }

    @Override
    public FlowchartBuilder buildControlFlow() {
        FlowNode rootFlowNode = graphRoot;
        rootFlowNode.accept(new ControlFlowVisitor(), 1);
        return this;
    }

    @Override
    public void generateFlowchart(ParseTree procedure, String dotFilePath, String imageOutputPath, String splineStyle) throws IOException, InterruptedException {
        buildFlowAST(procedure)
                .buildControlFlow()
                .buildOverlay()
//        .buildDotStructure(VisitContext.VISIT_UPTO_LEVEL(4)) // Level 4 for only sections and paragraphs
                .buildDotStructure()
                .write(dotFilePath);
        new GraphGenerator(splineStyle).generateImage(dotFilePath, imageOutputPath);
    }

    @Override
    public FlowNode getRoot() {
        return graphRoot;
    }

    @Override
    public FlowNodeService getChartNodeService() {
        return flowNodeService;
    }

    @Override
    public FlowchartBuilder accept(GraphWriter graphWriter) {
        graphWriter.process(graph, overlay);
        return this;
    }

    private String formatted(String s, int lineLength) {
        StringBuilder builder = new StringBuilder(s);
        int length = s.length();
        for (int i = lineLength; i < length; i += lineLength) {
            builder.insert(i, "\n");
        }
        return builder.toString();
    }

    private MutableNode commentStyle(MutableNode commentNode) {
        return commentNode.add("shape", Shape.BOX)
                .add("height", 4)
                .add("style", "filled")
                .add("fillcolor", Color.YELLOW.value);
    }

    public static FlowchartBuilder build(CobolEntityNavigator navigator, CobolDataStructure dataStructures) {
        return new FlowchartBuilderImpl(navigator, dataStructures);
    }
}
