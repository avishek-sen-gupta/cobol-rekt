package org.smojol.toolkit.analysis.graph;

import com.mojo.woof.*;
import org.neo4j.driver.Record;

import java.util.List;

import static com.mojo.woof.NodeAccess.source;

public class SummariseAction implements NodeAction {
    private final Advisor advisor;
    private final GraphSDK sdk;

    public SummariseAction(Advisor advisor, GraphSDK sdk) {
        this.advisor = advisor;
        this.sdk = sdk;
    }

    @Override
    public ActionResult apply(Record node, List<ActionResult> childResults) {
        List<String> childStrings = childResults.stream().map(ActionResult::toString).toList();
        String s = NodeAccess.type(node) + " composed of [" + String.join(",", childStrings) + "]";
        List<String> advice = advisor.advise("Summarise the following: " + source(node) + ", given the following child summaries: " + s);
        String summary = advice.stream().reduce("", (a, b) -> a + b);
        sdk.createSummary(summary, node);
        return new SummaryActionResult(summary);
    }
}
