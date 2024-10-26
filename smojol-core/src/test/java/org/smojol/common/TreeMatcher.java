package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;
import java.util.function.Predicate;

import static com.google.common.collect.Streams.zip;

public class TreeMatcher {
    private final List<TreeMatcher> childMatchers;
    private final Predicate<TranspilerNode> condition;

    public TreeMatcher(Predicate<TranspilerNode> condition, List<TreeMatcher> childMatchers) {
        this.condition = condition;
        this.childMatchers = childMatchers;
    }

    public TreeMatcher(Predicate<TranspilerNode> condition) {
        this(condition, ImmutableList.of());
    }

    public TreeMatchResult run(TranspilerNode node) {
        List<TreeMatchResult> childMatchResults = zip(node.astChildren().stream(), childMatchers.stream(), (child, matcher) -> matcher.run(child)).toList();
        return new TreeMatchResult(matchSelf(node), childMatchResults);
    }

    private TreeMatchDetail matchSelf(TranspilerNode node) {
        return new TreeMatchDetail(condition.test(node), node.astChildren().size() == childMatchers.size(), node);
    }
}
