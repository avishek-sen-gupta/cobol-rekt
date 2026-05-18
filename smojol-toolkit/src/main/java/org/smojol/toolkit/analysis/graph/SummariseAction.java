package org.smojol.toolkit.analysis.graph;

import static com.mojo.woof.NodeAccess.source;

import com.mojo.woof.*;
import com.mojo.woof.llm.Advisor;
import java.util.List;
import org.neo4j.driver.Record;

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
    List<String> advice =
        advisor.advise(
            "Summarise the following: "
                + source(node)
                + ", given the following child summaries: "
                + s);
    String summary = advice.stream().reduce("", (a, b) -> a + b);
    sdk.createSummary(summary, node);
    return new SummaryActionResult(summary);
  }
}
