package org.smojol.analysis.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.jgrapht.nio.graphml.GraphMLExporter;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.io.File;
import java.util.List;
import java.util.Map;

import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeRelations.*;

public class GraphMLExportCommands {
    private final CobolDataStructure data;
    private final NodeSpecBuilder qualifier;
    private final FlowNode root;
    private final Graph<FlowNode, TypedGraphMLEdge> astGraph;
    private final Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph;

    public GraphMLExportCommands(CobolDataStructure dataStructures, FlowNode root, NodeSpecBuilder qualifier) {
        this.data = dataStructures;
        this.qualifier = qualifier;
        this.root = root;
        astGraph = new DirectedAcyclicGraph<>(TypedGraphMLEdge.class);
        dataStructuresGraph = new DefaultDirectedGraph<>(TypedGraphMLEdge.class);
    }

    public void buildDataStructures() {
        GraphMLDataStructureVisitor graphMLDataStructuresExporter = new GraphMLDataStructureVisitor(dataStructuresGraph, qualifier);
        data.accept(graphMLDataStructuresExporter, null, n -> false, data);
        new FlowNodeASTTraversal<Boolean>().build(root, this::buildDataDependency);
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
        exporter.exportGraph(graphMLDataStructuresExporter.getDataStructuresGraph(), new File("/Users/asgupta/code/smojol/out/data_structures.graphml"));
    }

    public FlowNode buildGraphML(FlowNode node, FlowNode parent) {
        astGraph.addVertex(node);
        if (parent == null) return node;
        astGraph.addEdge(parent, node, new TypedGraphMLEdge(CONTAINS));
        return node;
    }

    public Boolean buildDataDependency(FlowNode node, Boolean parent) {
        Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, data);
        connect(pairs.getKey(), pairs.getValue());
        return true;
    }

    private void connect(List<CobolDataStructure> froms, List<CobolDataStructure> tos) {
        tos.forEach(to -> froms.forEach(from -> {
            if (!dataStructuresGraph.containsVertex(from)) dataStructuresGraph.addVertex(from);
            dataStructuresGraph.addEdge(from, to, new TypedGraphMLEdge(MODIFIES));
        }));
    }

    public void buildAST() {
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
        new FlowNodeASTTraversal<FlowNode>().build(root, this::buildGraphML);
        exporter.exportGraph(astGraph, new File("/Users/asgupta/code/smojol/out/ast.graphml"));
    }

    private Attribute attr(String attribute) {
        return new DefaultAttribute<>(attribute, AttributeType.STRING);
    }
}
