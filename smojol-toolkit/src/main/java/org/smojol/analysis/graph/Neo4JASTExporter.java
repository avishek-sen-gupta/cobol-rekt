package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
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

    public Record make(FlowNode tree, Record parent) {
        WoofNode node = new WoofNode(qualifier.newASTNode(tree));
        Record record = sdk.createNode(node);
        if (parent == null) return record;
        sdk.contains(parent, record);
        return record;
    }

    public Boolean buildDataDependency(FlowNode node, Boolean parent) {
        Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, data);
        connect(pairs.getKey(), pairs.getValue());
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
}
