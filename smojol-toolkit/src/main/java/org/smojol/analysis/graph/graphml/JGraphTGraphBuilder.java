package org.smojol.analysis.graph.graphml;

import lombok.Getter;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DirectedPseudograph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.jgrapht.nio.graphml.GraphMLExporter;
import org.smojol.analysis.graph.DataDependencyPairComputer;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.jgrapht.JGraphTDataOperations;
import org.smojol.analysis.graph.jgrapht.JGraphTCodeOperations;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.io.File;
import java.util.List;
import java.util.Map;

import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeRelations.*;

public class JGraphTGraphBuilder {
    private final CobolDataStructure dataRoot;
    private final NodeSpecBuilder qualifier;
    private final FlowNode astRoot;
    private final FlowNode cfgRoot;
    private final Graph<TypedGraphVertex, TypedGraphEdge> astGraph;
    private final Graph<TypedGraphVertex, TypedGraphEdge> cfgGraph;
    private final Graph<TypedGraphVertex, TypedGraphEdge> dataStructuresGraph;
    @Getter
    private final Graph<TypedGraphVertex, TypedGraphEdge> model;
    private final JGraphTCodeOperations astGraphOperations;
    private final JGraphTDataOperations dataGraphOperations;

    public JGraphTGraphBuilder(CobolDataStructure dataStructures, FlowNode procedureRoot, NodeSpecBuilder qualifier) {
        this.dataRoot = dataStructures;
        this.astRoot = this.cfgRoot = procedureRoot;
        this.qualifier = qualifier;
        model = cfgGraph = astGraph = dataStructuresGraph = new DirectedPseudograph<>(TypedGraphEdge.class);
//        astGraph = new DirectedAcyclicGraph<>(TypedGraphEdge.class);
//        cfgGraph = new DefaultDirectedGraph<>(TypedGraphEdge.class);
//        dataStructuresGraph = new DefaultDirectedGraph<>(TypedGraphEdge.class);
        astGraphOperations = new JGraphTCodeOperations(astGraph, qualifier);
        dataGraphOperations = new JGraphTDataOperations(dataStructuresGraph, qualifier);
    }

    public void buildAST() {
        new FlowNodeASTTraversal<FlowNode>().build(astRoot, this::buildJGraphTNodes);
    }

    public void buildCFG() {
        cfgRoot.accept(new GraphMLCFGVisitor(cfgGraph, qualifier), -1);
    }

    public void buildDataStructures() {
        dataRoot.accept(new GraphMLDataStructureVisitor(dataStructuresGraph, qualifier), null, n -> false, dataRoot);
        dataRoot.accept(new GraphMLRedefinitionVisitor(dataStructuresGraph, qualifier), null, n -> false, dataRoot);
        new FlowNodeASTTraversal<Boolean>().build(astRoot, this::buildDataDependency);
    }

    public FlowNode buildJGraphTNodes(FlowNode node, FlowNode parent) {
        astGraphOperations.addNode(node);
        if (parent == null) return node;
        astGraphOperations.connect(parent, node, CONTAINS);
        return node;
    }

    public Boolean buildDataDependency(FlowNode node, Boolean parent) {
        Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, dataRoot);
        if (ImmutablePair.nullPair().equals(pairs)) return false;
        if (pairs.getValue().isEmpty()) {
            accesses(node, pairs.getKey());
            return true;
        }
        connect(pairs.getKey(), pairs.getValue(), node);
        return true;
    }

    private void accesses(FlowNode node, List<CobolDataStructure> accessedStructures) {
        accessedStructures.forEach(s -> {
            if (!dataGraphOperations.containsVertex(s)) dataGraphOperations.addNode(s);
            dataGraphOperations.connect(node, s, ACCESSES);
        });

    }

    private void connect(List<CobolDataStructure> froms, List<CobolDataStructure> tos, FlowNode node) {
        tos.forEach(to -> {
            dataGraphOperations.connect(node, to, MODIFIES);
            froms.forEach(from -> {
                if (!dataGraphOperations.containsVertex(from)) dataGraphOperations.addNode(from);
                dataGraphOperations.connect(node, from, ACCESSES);
                dataGraphOperations.connect(from, to, FLOWS_INTO);
            });
        });
    }

    private Attribute attr(String attribute) {
        return new DefaultAttribute<>(attribute, AttributeType.STRING);
    }

    private void export(Graph<TypedGraphVertex, TypedGraphEdge> graph, File outputPath) {
        GraphMLExporter<TypedGraphVertex, TypedGraphEdge> exporter = new GraphMLExporter<>();
        exporter.registerAttribute(ID, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(TYPE, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(NAME, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(TEXT, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(LABEL, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute(NAMESPACE, GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
//        exporter.registerAttribute(NAMESPACE, GraphMLExporter.AttributeCategory.EDGE, AttributeType.STRING);
        exporter.registerAttribute(RELATIONSHIP_TYPE, GraphMLExporter.AttributeCategory.EDGE, AttributeType.STRING);
        exporter.setVertexAttributeProvider(s -> Map.of(
                ID, attr(s.id()),
                NAME, attr(s.name()),
                TYPE, attr(s.type()),
                LABEL, attr(s.label()),
                TEXT, attr(s.text()),
                NAMESPACE, attr(s.namespace())));

        exporter.setEdgeAttributeProvider(e -> Map.of(
                RELATIONSHIP_TYPE, attr(e.getRelationshipType()),
                NAMESPACE, attr(e.getNamespace())));
        exporter.setVertexIdProvider(TypedGraphVertex::id);
        exporter.exportGraph(graph, outputPath);
    }

    public void write(File outputPath) {
        export(astGraph, outputPath);
    }
}
