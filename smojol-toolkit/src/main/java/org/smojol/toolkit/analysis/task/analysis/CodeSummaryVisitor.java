package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.*;
import org.neo4j.driver.Record;
import org.smojol.common.ast.TreeMapperVisitor;

import java.util.List;

import static com.mojo.woof.NodeAccess.source;

public class CodeSummaryVisitor extends TreeMapperVisitor<Record, ActionResult> {
    private final Advisor advisor;
    private final GraphSDK sdk;

    public CodeSummaryVisitor(Advisor advisor, GraphSDK sdk) {
        super(null);
        this.advisor = advisor;
        this.sdk = sdk;
    }

    @Override
    public void visit(Record node) {

    }

    @Override
    public void enter(Record node) {

    }

    @Override
    public void exit(Record node) {

    }

    @Override
    public TreeMapperVisitor<Record, ActionResult> scope(Record n) {
        return this;
    }

    @Override
    public ActionResult processChildResults(Record node, List<ActionResult> childResults) {
        List<String> childStrings = childResults.stream().map(ActionResult::toString).toList();
        String s = NodeAccess.type(node) + " composed of [" + String.join(",", childStrings) + "]";
        List<String> advice = advisor.advise("Summarise the following: " + source(node) + ", given the following child summaries: " + s);
        String summary = advice.stream().reduce("", (a, b) -> a + b);
        sdk.createSummary(summary, node);
        return new SummaryActionResult(summary);
    }
}
