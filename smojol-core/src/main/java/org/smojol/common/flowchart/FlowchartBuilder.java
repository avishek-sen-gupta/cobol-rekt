package org.smojol.common.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;

import java.io.IOException;
import java.util.function.Function;

public interface FlowchartBuilder {
    FlowchartBuilder buildDotStructure(Function<VisitContext, Boolean> stopCondition);
    FlowchartBuilder buildDotStructure();
    FlowchartBuilder buildChartAST(ParseTree node);
    FlowchartBuilder write(String dotFilePath);
    FlowchartBuilder outline(ParseTree groupRoot, String clusterLabel);
    FlowchartBuilder connectToComment(String explanation, ParseTree symbol);
    FlowchartBuilder createComment(String comment);
    FlowchartBuilder buildOverlay();
    FlowchartBuilder buildControlFlow();
    FlowchartBuilder accept(GraphWriter graphWriter);
    void generateFlowchart(ParseTree procedure, String dotFilePath, String imageOutputPath, String splineStyle) throws IOException, InterruptedException;
    FlowNode getRoot();
    FlowNodeService getChartNodeService();
}
