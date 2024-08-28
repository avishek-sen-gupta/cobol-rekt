package org.smojol.common.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.VisitContext;

import java.io.IOException;
import java.util.function.Function;

public interface FlowchartBuilder {
    FlowchartBuilder buildDotStructure(Function<VisitContext, Boolean> stopCondition, FlowchartOutputFormat flowchartOutputFormat);
    FlowchartBuilder buildDotStructure(FlowchartOutputFormat flowchartOutputFormat);
    FlowchartBuilder buildFlowAST(ParseTree node);
    FlowchartBuilder write(String dotFilePath, FlowchartOutputFormat outputFormat);
    FlowchartBuilder outline(ParseTree groupRoot, String clusterLabel);
    FlowchartBuilder connectToComment(String explanation, ParseTree symbol);
    FlowchartBuilder createComment(String comment);
    FlowchartBuilder buildOverlay();
    FlowchartBuilder buildControlFlow();
    FlowchartBuilder accept(GraphWriter graphWriter);
    void generateFlowchart(ParseTree procedure, String dotFilePath, String imageOutputPath, FlowchartOutputFormat outputFormat) throws IOException, InterruptedException;
    FlowNode getRoot();
    FlowNodeService getChartNodeService();
}
