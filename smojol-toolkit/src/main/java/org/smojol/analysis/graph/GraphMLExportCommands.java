package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.jgrapht.nio.graphml.GraphMLExporter;
import org.smojol.ast.*;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.reference.ShallowReferenceBuilder;
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
        if (node.type() != FlowNodeType.MOVE
                && node.type() != FlowNodeType.COMPUTE
                && node.type() != FlowNodeType.ADD
                && node.type() != FlowNodeType.SUBTRACT
                && node.type() != FlowNodeType.MULTIPLY
                && node.type() != FlowNodeType.DIVIDE
        ) return false;

        ShallowReferenceBuilder referenceBuilder = new ShallowReferenceBuilder();
        if (node.type() == FlowNodeType.MOVE) {
            MoveFlowNode move = (MoveFlowNode) node;
            List<CobolDataStructure> froms = ImmutableList.of(referenceBuilder.getShallowReference(move.getFrom(), data).resolve());
            List<CobolDataStructure> tos = move.getTos().stream().map(t -> referenceBuilder.getShallowReference(t, data).resolve()).toList();
            connect(froms, tos);
        } else if (node.type() == FlowNodeType.COMPUTE) {
            ComputeFlowNode compute = (ComputeFlowNode) node;
            ArithmeticExpressionVisitor visitor = new ArithmeticExpressionVisitor();
            compute.getRhs().accept(visitor);
            CobolExpression expression = visitor.getExpression();
            StaticExpressionCollector expressionCollector = new StaticExpressionCollector(data);
            expression.accept(expressionCollector);
            List<CobolDataStructure> froms = expressionCollector.structures();
            List<CobolDataStructure> tos = compute.getDestinations().stream().map(d -> referenceBuilder.getShallowReference(d.generalIdentifier(), data).resolve()).toList();
            connect(froms, tos);
        } else if (node.type() == FlowNodeType.ADD) {
            AddFlowNode add = (AddFlowNode) node;
            List<CobolDataStructure> froms = add.getFroms().stream().map(f -> referenceBuilder.getShallowReference(f, data).resolve()).toList();
            List<CobolDataStructure> tos = add.getTos().stream().map(t -> referenceBuilder.getShallowReference(t, data).resolve()).toList();
            connect(froms, tos);
        } else if (node.type() == FlowNodeType.SUBTRACT) {
            SubtractFlowNode subtract = (SubtractFlowNode) node;
            List<CobolDataStructure> minuends = subtract.getLhs().stream().map(f -> referenceBuilder.getShallowReference(f, data).resolve()).toList();
            List<CobolDataStructure> subtrahends = subtract.getRhs().stream().map(t -> referenceBuilder.getShallowReference(t, data).resolve()).toList();
            connect(subtrahends, minuends);
        } else if (node.type() == FlowNodeType.MULTIPLY) {
            MultiplyFlowNode multiply = (MultiplyFlowNode) node;
            List<CobolDataStructure> lhses = ImmutableList.of(referenceBuilder.getShallowReference(multiply.getLhs(), data).resolve());
            List<CobolDataStructure> rhses = multiply.getRhs().stream().map(t -> referenceBuilder.getShallowReference(t.generalIdentifier(), data).resolve()).toList();
            connect(lhses, rhses);
        } else if (node.type() == FlowNodeType.DIVIDE) {
            DivideFlowNode divide = (DivideFlowNode) node;
            List<CobolDataStructure> divisors = ImmutableList.of(referenceBuilder.getShallowReference(divide.getDivisor(), data).resolve());
            List<CobolDataStructure> dividends = divide.getDividends().stream().map(t -> referenceBuilder.getShallowReference(t.generalIdentifier(), data).resolve()).toList();
            connect(divisors, dividends);
        }
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
