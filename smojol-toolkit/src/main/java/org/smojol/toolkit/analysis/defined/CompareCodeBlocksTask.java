package org.smojol.toolkit.analysis.defined;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.similarity.ZhangShashaTreeEditDistance;
import org.jgrapht.graph.DefaultUndirectedGraph;
import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.NodeOperationCostFunctions;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.JGraphTGraphBuilder;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.toolkit.ast.ParagraphFlowNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

public class CompareCodeBlocksTask {
    private static final Logger LOGGER = Logger.getLogger(CompareCodeBlocksTask.class.getName());
    private final CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;

    public CompareCodeBlocksTask(CobolDataStructure dataStructures, NodeSpecBuilder qualifier) {
        this.dataStructures = dataStructures;
        this.qualifier = qualifier;
    }

    public AnalysisTaskResult run(FlowNode astRoot, NodeOperationCostFunctions costFunctionBlock) {
        List<FlowNode> sectionNodes = new FlowNodeNavigator(astRoot).findAllByCondition(n -> n.getClass() == ParagraphFlowNode.class);
        HashMap<FlowNode, Pair<TypedGraphVertex, Graph<TypedGraphVertex, TypedGraphEdge>>> flowJGraphTPairs = new HashMap<>();
        sectionNodes.forEach(n -> {
            JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, n, qualifier, new DefaultUndirectedGraph<>(TypedGraphEdge.class));
            graphMLExporter.buildAST();
            Graph<TypedGraphVertex, TypedGraphEdge> model = graphMLExporter.getModel();
            TypedGraphVertex root = model.vertexSet().stream().filter(v -> FlowNodeType.PARAGRAPH.name().equals(v.type())).toList().getFirst();
            flowJGraphTPairs.put(n, ImmutablePair.of(root, model));
        });

        List<Pair<FlowNode, FlowNode>> allComparandFlowPairs = recurse(sectionNodes);
        List<SimilarityResult> allDistances = allComparandFlowPairs.stream().map(p -> {
            Pair<TypedGraphVertex, Graph<TypedGraphVertex, TypedGraphEdge>> leftGraphTModel = flowJGraphTPairs.get(p.getLeft());
            Pair<TypedGraphVertex, Graph<TypedGraphVertex, TypedGraphEdge>> rightGraphTModel = flowJGraphTPairs.get(p.getRight());
            Graph<TypedGraphVertex, TypedGraphEdge> leftModel = leftGraphTModel.getRight();
            Graph<TypedGraphVertex, TypedGraphEdge> rightModel = rightGraphTModel.getRight();
            TypedGraphVertex leftRoot = leftGraphTModel.getLeft();
            TypedGraphVertex rightRoot = rightGraphTModel.getLeft();
            ZhangShashaTreeEditDistance<TypedGraphVertex, TypedGraphEdge> editDistance =
                    new ZhangShashaTreeEditDistance<>(leftModel, leftRoot, rightModel, rightRoot,
                            costFunctionBlock.add(), costFunctionBlock.remove(), costFunctionBlock.change());
//            return ImmutablePair.of(editDistance.getEditOperationLists(), editDistance.getDistance());
            return new SimilarityResult(p, editDistance.getDistance(), editDistance.getEditOperationLists());
        }).toList();
        allDistances.forEach(d -> {
            LOGGER.info(String.format("Distance between %s and %s is %s",
                    d.nodes().getLeft(),
                    d.nodes().getRight(),
                    d.distance()));
        });
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.COMPARE_CODE, allDistances);
    }

    private List<Pair<FlowNode, FlowNode>> recurse(List<FlowNode> paragraphs) {
        if (paragraphs.size() <= 1) return ImmutableList.of();
        List<ImmutablePair<FlowNode, FlowNode>> comparands =
                paragraphs.subList(1, paragraphs.size()).stream().map(p -> ImmutablePair.of(paragraphs.getFirst(), p)).toList();
        List<Pair<FlowNode, FlowNode>> allComparands = new ArrayList<>();
        List<Pair<FlowNode, FlowNode>> recursedComparands = recurse(paragraphs.subList(1, paragraphs.size()));
        allComparands.addAll(comparands);
        allComparands.addAll(recursedComparands);
        return allComparands;
    }
}
