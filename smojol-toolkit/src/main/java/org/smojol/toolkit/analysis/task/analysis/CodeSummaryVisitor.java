package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.Advisor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.TreeMapperVisitor;

import java.util.List;

public class CodeSummaryVisitor extends TreeMapperVisitor<FlowNode, SummaryTree> {
    private final Advisor advisor;

    public CodeSummaryVisitor(Advisor advisor) {
        super(null);
        this.advisor = advisor;
    }

    @Override
    public void visit(FlowNode node) {

    }

    @Override
    public void enter(FlowNode node) {

    }

    @Override
    public void exit(FlowNode node) {

    }

    @Override
    public TreeMapperVisitor<FlowNode, SummaryTree> scope(FlowNode n) {
        return this;
    }

    @Override
    public SummaryTree processChildResults(FlowNode node, List<SummaryTree> mappedChildren) {
        List<String> childStrings = mappedChildren.stream().map(SummaryTree::toString).toList();
        String s = node.type() + " composed of [" + String.join(",", childStrings) + "]";
        List<String> advice = advisor.advise("Summarise the following: " + node.originalText() + ", given the following child summaries: " + s);
        String summary = advice.stream().reduce("", (a, b) -> a + b);
        return new SummaryTree(summary, mappedChildren);
    }
}
