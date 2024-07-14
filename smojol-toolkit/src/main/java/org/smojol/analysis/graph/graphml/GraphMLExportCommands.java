package org.smojol.analysis.graph.graphml;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.jgrapht.nio.graphml.GraphMLExporter;
import org.smojol.analysis.graph.DataDependencyPairComputer;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.jgrapht.JGraphTDataOperations;
import org.smojol.analysis.graph.jgrapht.JGraphTOperations;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.io.File;
import java.util.List;
import java.util.Map;

import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeRelations.*;

public class GraphMLExportCommands {
    private final CobolDataStructure dataRoot;
    private final NodeSpecBuilder qualifier;
    private final FlowNode astRoot;
    private final FlowNode cfgRoot;
    private final Graph<FlowNode, TypedGraphMLEdge> astGraph;
    private final Graph<FlowNode, TypedGraphMLEdge> cfgGraph;
    private final Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph;
    private final JGraphTOperations astGraphOperations;
    private final JGraphTDataOperations dataGraphOperations;

    public GraphMLExportCommands(CobolDataStructure dataStructures, FlowNode procedureRoot, NodeSpecBuilder qualifier) {
        this.dataRoot = dataStructures;
        this.astRoot = this.cfgRoot = procedureRoot;
        this.qualifier = qualifier;
        astGraph = new DirectedAcyclicGraph<>(TypedGraphMLEdge.class);
        cfgGraph = new DefaultDirectedGraph<>(TypedGraphMLEdge.class);
        dataStructuresGraph = new DefaultDirectedGraph<>(TypedGraphMLEdge.class);
        astGraphOperations = new JGraphTOperations(astGraph);
        dataGraphOperations = new JGraphTDataOperations(dataStructuresGraph);
    }

    public void buildDataStructures(File outputPath) {
        dataRoot.accept(new GraphMLDataStructureVisitor(dataStructuresGraph, qualifier), null, n -> false, dataRoot);
        dataRoot.accept(new GraphMLRedefinitionVisitor(dataStructuresGraph, qualifier), null, n -> false, dataRoot);
        new FlowNodeASTTraversal<Boolean>().build(astRoot, this::buildDataDependency);
        export(dataStructuresGraph, outputPath);
    }

    private void export(Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph, File outputPath) {
        GraphMLExporter<CobolDataStructure, TypedGraphMLEdge> exporter = new GraphMLExporter<>();
        exporter.registerAttribute(TYPE, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(NAME, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(TEXT, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(RELATIONSHIP_TYPE, GraphMLExporter.AttributeCategory.EDGE, AttributeType.STRING);
        exporter.setVertexAttributeProvider(s -> Map.of(
                ID, attr(s.getId()),
                NAME, attr(s.name()),
                TYPE, attr(s.getDataType().name()),
                TEXT, attr(s.content())));

        exporter.setEdgeAttributeProvider(e -> Map.of(RELATIONSHIP_TYPE, attr(e.getRelationshipType())));
        exporter.setVertexIdProvider(CobolDataStructure::getId);
        exporter.exportGraph(dataStructuresGraph, outputPath);
    }

    public FlowNode buildGraphML(FlowNode node, FlowNode parent) {
        astGraphOperations.addNode(node);
        if (parent == null) return node;
        astGraphOperations.connect(parent, node, CONTAINS);
        return node;
    }

    public Boolean buildDataDependency(FlowNode node, Boolean parent) {
        Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, dataRoot);
        if (ImmutablePair.nullPair().equals(pairs)) return false;
        connect(pairs.getKey(), pairs.getValue());
        return true;
    }

    private void connect(List<CobolDataStructure> froms, List<CobolDataStructure> tos) {
        tos.forEach(to -> froms.forEach(from -> {
            if (!dataGraphOperations.containsVertex(from)) dataGraphOperations.addNode(from);
            dataGraphOperations.connect(from, to, MODIFIES);
        }));
    }

    public void buildAST(File outputPath) {
        new FlowNodeASTTraversal<FlowNode>().build(astRoot, this::buildGraphML);
        GraphMLExporter<FlowNode, TypedGraphMLEdge> exporter = new GraphMLExporter<>();
        exporter.registerAttribute(TYPE, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(NAME, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(TEXT, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(RELATIONSHIP_TYPE, GraphMLExporter.AttributeCategory.EDGE, AttributeType.STRING);
        exporter.setVertexAttributeProvider(n -> Map.of(
                ID, attr(n.id()),
                TYPE, attr(n.type().toString()),
                NAME, attr(n.label()),
                TEXT, attr(n.originalText())));

        exporter.setEdgeAttributeProvider(e -> Map.of(RELATIONSHIP_TYPE, attr(e.getRelationshipType())));
        exporter.setVertexIdProvider(FlowNode::id);
        exporter.exportGraph(astGraph, outputPath);
    }

    private Attribute attr(String attribute) {
        return new DefaultAttribute<>(attribute, AttributeType.STRING);
    }

    public void buildCFG(File outputPath) {
        cfgRoot.accept(new GraphMLCFGVisitor(cfgGraph, qualifier), -1);
        GraphMLExporter<FlowNode, TypedGraphMLEdge> exporter = new GraphMLExporter<>();
        exporter.registerAttribute(TYPE, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(NAME, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(TEXT, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(RELATIONSHIP_TYPE, GraphMLExporter.AttributeCategory.EDGE, AttributeType.STRING);
        exporter.setVertexAttributeProvider(n -> Map.of(
                ID, attr(n.id()),
                TYPE, attr(n.type().toString()),
                NAME, attr(n.label()),
                TEXT, attr(n.originalText())));

        exporter.setEdgeAttributeProvider(e -> Map.of(RELATIONSHIP_TYPE, attr(e.getRelationshipType())));
        exporter.setVertexIdProvider(FlowNode::id);
        exporter.exportGraph(cfgGraph, outputPath);
    }

}
