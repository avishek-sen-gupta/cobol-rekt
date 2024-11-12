package org.smojol.toolkit.analysis.task.analysis;

import java.util.List;

public record SummaryTree(String summary, List<SummaryTree> children) {

    @Override
    public String toString() {
        return summary;
    }
}
