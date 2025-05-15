package org.smojol.toolkit.analysis.task.analysis;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.*;
import org.neo4j.driver.Record;
import com.mojo.algorithms.navigation.TreeMapperVisitor;

import java.util.List;
import java.util.logging.Logger;

public class Neo4JDataSummaryVisitor extends TreeMapperVisitor<Record, ActionResult> {
    private static final Logger LOGGER = Logger.getLogger(Neo4JDataSummaryVisitor.class.getName());
    private final Advisor advisor;
    private final GraphSDK sdk;

    public Neo4JDataSummaryVisitor(Advisor advisor, GraphSDK sdk) {
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
    public ActionResult processChildResults(Record node, List<ActionResult> mappedChildren) {
        List<String> childStrings = mappedChildren.stream().map(ActionResult::toString).toList();
        String s = NodeAccess.name(node) + " is of type " + NodeAccess.type(node) + " and is composed of [" + String.join(",", childStrings) + "]";
        String prompt = "You are a domain expert. This is a variable associated with workflows in this domain. Without any extra text, deduce what this variable represents. Be as precise as possible. Summaries of child data structures follow: " + s;
        LOGGER.info("Prompt is : " + prompt);
        List<String> advice = advisor.advise(prompt);
        String summary = advice.stream().reduce("", (a, b) -> a + b);
//        List<String> domains = Arrays.asList(summary.split(","));
//        domains.forEach(domain -> sdk.createSummary(domain, node));
        sdk.createSummary(summary, node);
        return new DataStructureSummaryActionResult(ImmutableList.of(summary));
    }
}
