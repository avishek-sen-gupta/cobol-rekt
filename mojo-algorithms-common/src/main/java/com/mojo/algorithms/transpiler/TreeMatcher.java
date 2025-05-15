package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerLoop;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.domain.TreeMatchResult;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

import static com.google.common.collect.Streams.zip;

public class TreeMatcher {
    private final List<TreeMatcher> childMatchers;
    private final boolean isTerminal;
    private final Predicate<TranspilerNode> condition;

    public TreeMatcher(Predicate<TranspilerNode> condition, List<TreeMatcher> childMatchers) {
        this(condition, childMatchers, false);
    }

    private TreeMatcher(Predicate<TranspilerNode> condition, List<TreeMatcher> childMatchers, boolean isTerminal) {
        this.condition = condition;
        this.childMatchers = childMatchers;
        this.isTerminal = isTerminal;
    }

    public TreeMatcher(Predicate<TranspilerNode> condition) {
        this(condition, ImmutableList.of(), true);
    }

    public TreeMatchResult run(TranspilerNode node) {
        if (isTerminal) return new TreeMatchResult(matchSelf(node), ImmutableList.of());
        List<TreeMatchResult> childMatchResults = zip(node.astChildren().stream(), childMatchers.stream(), (child, matcher) -> matcher.run(child)).toList();
        return new TreeMatchResult(matchSelf(node), childMatchResults);
    }

    private TreeMatchDetail matchSelf(TranspilerNode node) {
        return new TreeMatchDetail(condition.test(node), node.astChildren().size() == childMatchers.size(), node);
    }

    public static TreeMatcher jmpIf_() {
        return new TreeMatcher(n -> n instanceof JumpIfTranspilerNode);
    }

    public static TreeMatcher jmp_() {
        return new TreeMatcher(n -> n instanceof JumpTranspilerNode);
    }

    public static TreeMatcher any_() {
        return new TreeMatcher(n -> n != null);
    }

    public static TreeMatcher if_(TreeMatcher ifThen, TreeMatcher ifElse) {
        return new TreeMatcher(n -> n instanceof IfTranspilerNode, ImmutableList.of(ifThen, ifElse));
    }

    public static TreeMatcher set_() {
        return new TreeMatcher(n -> n instanceof SetTranspilerNode);
    }

    public static TreeMatcher print_() {
        return new TreeMatcher(n -> n instanceof PrintTranspilerNode);
    }

    public static TreeMatcher jump_() {
        return new TreeMatcher(n -> n instanceof JumpTranspilerNode);
    }

    public static TreeMatcher block_(TreeMatcher... childMatchers) {
        return new TreeMatcher(n -> n instanceof TranspilerCodeBlockNode, Arrays.asList(childMatchers));
    }

    public static TreeMatcher labelledBlock_(TreeMatcher... childMatchers) {
        return new TreeMatcher(n -> n instanceof LabelledTranspilerCodeBlockNode, Arrays.asList(childMatchers));
    }

    public static TreeMatcher labelledBlock_(String blockName, TreeMatcher... childMatchers) {
        return new TreeMatcher(n -> n instanceof LabelledTranspilerCodeBlockNode l && blockName.equals(l.getName()), Arrays.asList(childMatchers));
    }

    public static TreeMatcher loop_(TreeMatcher... childMatchers) {
        return new TreeMatcher(n -> n instanceof TranspilerLoop, Arrays.asList(childMatchers));
    }

    public void verify(TranspilerNode node) {
        run(node).assertStructure();
    }
}
