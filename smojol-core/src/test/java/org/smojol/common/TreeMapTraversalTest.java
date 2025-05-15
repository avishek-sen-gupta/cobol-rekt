package org.smojol.common;

import com.mojo.algorithms.navigation.TreeMapperTraversal;
import org.junit.jupiter.api.Test;
import com.mojo.algorithms.navigation.TreeMapperVisitor;
import com.mojo.algorithms.id.Identifiable;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TreeMapTraversalTest {
    @Test
    public void canMapTree() {
        TestTreeNode root = t("A", t("X"), t("Y"), t("Z"));
        SummarisedTestTreeNode mappedRoot = new TreeMapperTraversal<TestTreeNode, SummarisedTestTreeNode>().accept(root, new NodeSummariser(), TestTreeNode::children);
        assertEquals("A", mappedRoot.id());
        assertEquals(3, mappedRoot.children().size());
        assertEquals("X", mappedRoot.children().getFirst().id());
        assertEquals("Y", mappedRoot.children().get(1).id());
        assertEquals("Z", mappedRoot.children().get(2).id());
    }

    private TestTreeNode t(String id, TestTreeNode... nodes) {
        return new TestTreeNode(id, Arrays.asList(nodes));
    }
}

class NodeSummariser extends TreeMapperVisitor<TestTreeNode, SummarisedTestTreeNode> {

    NodeSummariser() {
        super(null);
    }

    @Override
    public void visit(TestTreeNode node) {

    }

    @Override
    public void enter(TestTreeNode node) {

    }

    @Override
    public void exit(TestTreeNode node) {

    }

    @Override
    public TreeMapperVisitor<TestTreeNode, SummarisedTestTreeNode> scope(TestTreeNode n) {
        return this;
    }

    @Override
    public SummarisedTestTreeNode processChildResults(TestTreeNode node, List<SummarisedTestTreeNode> mappedChildren) {
        return new SummarisedTestTreeNode(node.id(), "SOME SUMMARY OF " + node.label(), mappedChildren);
    }
}

record TestTreeNode(String id, List<TestTreeNode> children) implements Identifiable {
    @Override
    public String label() {
        return id;
    }

    @Override
    public String toString() {
        return id;
    }
}

record SummarisedTestTreeNode(String id, String summary, List<SummarisedTestTreeNode> children) implements Identifiable {
    @Override
    public String label() {
        return id;
    }

    @Override
    public String toString() {
        return id;
    }
}
