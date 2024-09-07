package org.smojol.toolkit.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.*;
import org.neo4j.driver.Record;
import org.smojol.toolkit.analysis.defined.ProgramDependenciesTask;

import java.util.List;
import java.util.logging.Logger;

public class DataStructureSummariseAction implements NodeAction {
    java.util.logging.Logger LOGGER = Logger.getLogger(DataStructureSummariseAction.class.getName());
    private final Advisor advisor;
    private final GraphSDK sdk;

    public DataStructureSummariseAction(Advisor advisor, GraphSDK sdk) {
        this.advisor = advisor;
        this.sdk = sdk;
    }

    @Override
    public ActionResult apply(Record node, List<ActionResult> childResults) {
        List<String> childStrings = childResults.stream().map(ActionResult::toString).toList();
        String s = NodeAccess.name(node) + " is of type " + NodeAccess.type(node) + " and is composed of [" + String.join(",", childStrings) + "]";
        String prompt = "You are an automotive domain expert. This is a variable associated with workflows in this domain. Without any extra text, deduce what this variable represents. Be as precise as possible. Summaries of child data structures follow: " + s;
        LOGGER.info("Prompt is : " + prompt);
        List<String> advice = advisor.advise(prompt);
        String summary = advice.stream().reduce("", (a, b) -> a + b);
//        List<String> domains = Arrays.asList(summary.split(","));
//        domains.forEach(domain -> sdk.createSummary(domain, node));
        sdk.createSummary(summary, node);
        return new DataStructureSummaryActionResult(ImmutableList.of(summary));
    }
}
