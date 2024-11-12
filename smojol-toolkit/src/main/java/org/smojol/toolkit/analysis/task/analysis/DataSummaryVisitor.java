package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.Advisor;
import org.smojol.common.ast.TreeMapperVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

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
        return new SummaryTree(summary, mappedChildren);
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
        return null;
    }
}
