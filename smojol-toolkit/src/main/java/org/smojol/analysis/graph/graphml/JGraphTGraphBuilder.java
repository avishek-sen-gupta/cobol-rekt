package org.smojol.analysis.graph.graphml;

import lombok.Getter;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.jgrapht.alg.clique.BronKerboschCliqueFinder;
import org.jgrapht.alg.clustering.KSpanningTreeClustering;
import org.jgrapht.alg.interfaces.ClusteringAlgorithm;
import org.jgrapht.graph.AsUndirectedGraph;
import org.jgrapht.graph.DirectedPseudograph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.jgrapht.nio.graphml.GraphMLExporter;
import org.smojol.analysis.graph.DataDependencyPairComputer;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.jgrapht.JGraphTDataOperations;
import org.smojol.analysis.graph.jgrapht.JGraphTCodeOperations;
import org.smojol.analysis.pipeline.SerialisableCobolDataStructure;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.SerialisableCFGFEdge;
import org.smojol.common.ast.SerialisableCFGFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.io.File;
import java.util.*;

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
        this(dataStructures, procedureRoot, qualifier, new DirectedPseudograph<>(TypedGraphEdge.class));
    }

    public JGraphTGraphBuilder(CobolDataStructure dataStructures, FlowNode procedureRoot, NodeSpecBuilder qualifier, Graph<TypedGraphVertex, TypedGraphEdge> graph) {
        this.dataRoot = dataStructures;
        this.astRoot = this.cfgRoot = procedureRoot;
        this.qualifier = qualifier;
        model = cfgGraph = astGraph = dataStructuresGraph = graph;
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
        astGraphOperations.connect(parent, node, CONTAINS_CODE);
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

    public SerialisableUnifiedModel writeToJSON() {
        List<SerialisableCFGFlowNode> serialisableCodeVertices = astGraph.vertexSet()
                .stream()
                .filter(v -> v.getClass() == TypedCodeVertex.class)
                .map(c -> new SerialisableCFGFlowNode(((TypedCodeVertex) c).getNode())).toList();
        List<SerialisableCobolDataStructure> serialisableDataVertices = astGraph.vertexSet()
                .stream()
                .filter(v -> v.getClass() == TypedDataStructureVertex.class)
                .map(c -> new SerialisableCobolDataStructure(((TypedDataStructureVertex) c).getNode())).toList();
//        List<Object> serialisableNodes = astGraph.vertexSet().stream().map(v -> {
//            return switch (v) {
//                case TypedCodeVertex c -> new SerialisableCFGFlowNode(c.getNode());
//                case TypedDataStructureVertex d -> new SerialisableCobolDataStructure(d.getNode());
//                default -> v;
//            };
//        }).toList();
        List<SerialisableCFGFEdge> serialisableEdges = astGraph.edgeSet().stream().map(e -> {
            TypedGraphVertex from = astGraph.getEdgeSource(e);
            TypedGraphVertex to = astGraph.getEdgeTarget(e);
            return new SerialisableCFGFEdge(UUID.randomUUID().toString(),
                    from.id(), to.id(), e.getRelationshipType());
        }).toList();
        return new SerialisableUnifiedModel(serialisableCodeVertices, serialisableDataVertices, serialisableEdges);
//        return ImmutableList.of(serialisableNodes, edges);
    }

    public void writeToGraphML(File outputPath) {
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
        exporter.exportGraph(astGraph, outputPath);
    }

    public void runAlgo(File outputPath) {
        AsUndirectedGraph<TypedGraphVertex, TypedGraphEdge> undirected = new AsUndirectedGraph<>(model);
        BronKerboschCliqueFinder<TypedGraphVertex, TypedGraphEdge> cliqueFinder = new BronKerboschCliqueFinder<>(undirected);
        ArrayList<Set<TypedGraphVertex>> cliques = new ArrayList<>();
        cliqueFinder.forEach(cliques::add);
        KSpanningTreeClustering<TypedGraphVertex, TypedGraphEdge> clusterer = new KSpanningTreeClustering<>(undirected, 50);
        ClusteringAlgorithm.Clustering<TypedGraphVertex> clustering = clusterer.getClustering();
        System.out.println("BOOM");
    }
}
