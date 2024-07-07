package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.NodeLabels;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.reference.ShallowReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.mojo.woof.NodeLabels.AST_NODE;
import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeRelations.CONTAINS;
import static com.mojo.woof.NodeRelations.MODIFIES;

public class Neo4JASTWalker {
    private final GraphSDK sdk;
    private final CobolDataStructure data;

    public Neo4JASTWalker(GraphSDK sdk, CobolDataStructure dataStructures) {
        this.sdk = sdk;
        this.data = dataStructures;
    }

    public void buildAST(FlowNode node) {
        new FlowNodeASTTraversal<Record>().build(node, this::make);
    }

    public Record make(FlowNode tree, Record parent) {
        WoofNode node = new WoofNode(Map.of(FLOW_ID, UUID.randomUUID().toString(),
                TEXT, NodeText.originalText(tree.getExecutionContext(), NodeText::PASSTHROUGH),
                TYPE, tree.type().toString()),
                ImmutableList.of(AST_NODE, tree.type().toString()));
        Record record = sdk.createNode(node);
        if (parent == null) return record;
        sdk.connect(parent, record, CONTAINS);
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
//            Record n4jFrom = sdk.newOrExisting(ImmutableList.of(NodeLabels.DATA_STRUCTURE), Map.of(NAME, froms.name()), dataStructureToWoof(froms));
            List<CobolDataStructure> tos = move.getTos().stream().map(t -> referenceBuilder.getShallowReference(t, data).resolve()).toList();
            connect(froms, tos);
//            tos.forEach(to -> {
//                Record n4jTo = sdk.findNode(ImmutableList.of(NodeLabels.DATA_STRUCTURE), Map.of(NAME, to.name())).getFirst();
//                sdk.connect(n4jTo, n4jFrom, MODIFIED_BY);
//            });
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
            Record n4jTo = sdk.findNode(ImmutableList.of(NodeLabels.DATA_STRUCTURE), Map.of(NAME, to.name())).getFirst();
            Record n4jFrom = sdk.newOrExisting(ImmutableList.of(NodeLabels.DATA_STRUCTURE), Map.of(NAME, from.name()),
                    NodeToWoof.dataStructureToWoof(from));
            sdk.connect(n4jFrom, n4jTo, MODIFIES);
        }));
    }

    private Boolean stopAtSentence(FlowNode tree) {
        return tree.type() == FlowNodeType.SECTION;
    }

    public void buildDataDependencies(FlowNode root) {
        new FlowNodeASTTraversal<Boolean>().build(root, this::buildDataDependency);
    }
}
