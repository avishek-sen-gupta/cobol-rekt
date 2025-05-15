package org.smojol.toolkit.analysis.task.analysis;

import com.google.common.collect.ImmutableMap;
import com.mojo.woof.Advisor;
import com.mojo.algorithms.navigation.TreeMapperVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;
import java.util.Map;

public class DataSummaryVisitor extends TreeMapperVisitor<CobolDataStructure, SummaryTree> {
    private final Advisor advisor;

    public DataSummaryVisitor(Advisor advisor) {
        super(null);
        this.advisor = advisor;
    }

    @Override
    public SummaryTree processChildResults(CobolDataStructure node, List<SummaryTree> mappedChildren) {
        List<String> childStrings = mappedChildren.stream().map(SummaryTree::toString).toList();
        String s = node.name() + " composed of [" + String.join(",", childStrings) + "]";
        List<String> advice = advisor.advise("Summarise the following: " + node.content() + ", given the following child summaries: " + s);
        String summary = advice.stream().reduce("", (a, b) -> a + b);
        return new SummaryTree(summary, asMap(node), mappedChildren);
    }

    private Map<String, String> asMap(CobolDataStructure node) {
        return ImmutableMap.of(
                "id", node.getId(),
                "text", node.name(),
                "type", node.getDataType().abstractType().name()
        );
    }

    @Override
    public void visit(CobolDataStructure node) {

    }

    @Override
    public void enter(CobolDataStructure node) {

    }

    @Override
    public void exit(CobolDataStructure node) {

    }

    @Override
    public TreeMapperVisitor<CobolDataStructure, SummaryTree> scope(CobolDataStructure n) {
        return this;
    }
}
