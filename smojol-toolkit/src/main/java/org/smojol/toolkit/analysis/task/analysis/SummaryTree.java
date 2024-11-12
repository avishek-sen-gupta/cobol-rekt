package org.smojol.toolkit.analysis.task.analysis;

import java.util.List;
import java.util.Map;

public record SummaryTree(String summary, Map<String, String> properties, List<SummaryTree> children) {

    @Override
    public String toString() {
        return summary;
    }
}
