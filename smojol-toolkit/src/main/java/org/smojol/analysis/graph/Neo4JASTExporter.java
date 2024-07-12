package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.jgrapht.nio.graphml.GraphMLExporter;
import org.neo4j.driver.Record;
import org.smojol.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.reference.ShallowReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.io.File;
import java.util.List;
import java.util.Map;

import static com.mojo.woof.NodeRelations.CONTAINS;

public class Neo4JASTExporter {
    private final GraphSDK sdk;
    private final CobolDataStructure data;
    private final NodeSpecBuilder qualifier;
    private Graph<FlowNode, DefaultEdge> graph;

    public Neo4JASTExporter(GraphSDK sdk, CobolDataStructure dataStructures, NodeSpecBuilder qualifier) {
        this.sdk = sdk;
        this.data = dataStructures;
        this.qualifier = qualifier;
    }

    public void buildAST(FlowNode node) {
        new FlowNodeASTTraversal<Record>().build(node, this::make);
    }

    public void buildDataDependencies(FlowNode root) {
        new FlowNodeASTTraversal<Boolean>().build(root, this::buildDataDependency);
    }

    public FlowNode buildGraphML(FlowNode node, FlowNode parent) {
        graph.addVertex(node);
        if (parent == null) return node;
        graph.addEdge(parent, node);
        return node;
    }

    public Record make(FlowNode tree, Record parent) {
        WoofNode node = new WoofNode(qualifier.newASTNode(tree));
        Record record = sdk.createNode(node);
        if (parent == null) return record;
        sdk.contains(parent, record);
        return record;
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
            Record n4jTo = sdk.findNodes(qualifier.dataNodeSearchSpec(to)).getFirst();
            Record n4jFrom = sdk.newOrExisting(qualifier.dataNodeSearchSpec(from), NodeToWoof.dataStructureToWoof(from, qualifier));
            sdk.modifies(n4jFrom, n4jTo);
        }));
    }

    private Boolean stopAtSentence(FlowNode tree) {
        return tree.type() == FlowNodeType.SECTION;
    }

    public void export(FlowNode root) {
        graph = new DirectedAcyclicGraph<>(DefaultEdge.class);
        GraphMLExporter<FlowNode, DefaultEdge> exporter = new GraphMLExporter<>();
        exporter.registerAttribute("type", GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute("label", GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute("text", GraphMLExporter.AttributeCategory.NODE, AttributeType.STRING);
        exporter.registerAttribute("relationshipType", GraphMLExporter.AttributeCategory.EDGE, AttributeType.STRING);
        exporter.setVertexAttributeProvider(n -> Map.of(
                "id", attr(n.id()),
                "type", attr(n.type().toString()),
                "label", attr(n.label()),
                "text", attr(n.originalText())));

        exporter.setEdgeAttributeProvider(e -> Map.of("relationshipType", attr(CONTAINS)));
        exporter.setVertexIdProvider(FlowNode::id);
        new FlowNodeASTTraversal<FlowNode>().build(root, this::buildGraphML);
        exporter.exportGraph(graph, new File("/Users/asgupta/code/smojol/out/test.graphml"));
    }

    private Attribute attr(String attribute) {
        return new DefaultAttribute<>(attribute, AttributeType.STRING);
    }
}
