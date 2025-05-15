package com.mojo.algorithms.domain;

import com.mojo.algorithms.transpiler.TreeMatchDetail;

import java.util.List;

public record TreeMatchResult(TreeMatchDetail selfMatchDetails, List<TreeMatchResult> childMatchResults) {
    public void assertStructure() {
        if (!selfMatchDetails.matched()) {
            throw new RuntimeException("Self details did not match for node: " + selfMatchDetails.actual().description());
        } else if (!selfMatchDetails().childMatcherCardinalitiesMatched()) {
            throw new RuntimeException("Cardinality mismatch for node: " + selfMatchDetails.actual().description());
        }
        childMatchResults.forEach(TreeMatchResult::assertStructure);
    }
}
