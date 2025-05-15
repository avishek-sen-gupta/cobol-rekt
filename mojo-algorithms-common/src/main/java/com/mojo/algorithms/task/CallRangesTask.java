package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.domain.TranspilerInstruction;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CallRangesTask {
    private final TranspilerNode tree;
    private final List<TranspilerInstruction> instructions;
    private final TranspilerNode mainNode;

    public CallRangesTask(TranspilerNode tree, List<TranspilerInstruction> instructions, TranspilerNode mainNode) {
        this.tree = tree;
        this.instructions = instructions;
        this.mainNode = mainNode;
    }

    public CallRangesTask(TranspilerCodeBlockNode program, List<TranspilerInstruction> instructions) {
        this(program, instructions, new NullTranspilerNode());
    }

    public Set<Pair<TranspilerInstruction, TranspilerInstruction>> run() {
        Set<TranspilerNode> allCalls = findAllRecursive(tree, n -> n instanceof JumpTranspilerNode l && l.getEnd() != LocationNode.NULL).stream().collect(Collectors.toUnmodifiableSet());
        Stream<Pair<TranspilerInstruction, TranspilerInstruction>> programLevelRange = mainNode instanceof NullTranspilerNode
                ? Stream.empty()
                : Stream.of(ImmutablePair.of(
                entryOrExitVertex(mainNode, CodeSentinelType.ENTER),
                entryOrExitVertex(mainNode, CodeSentinelType.EXIT)));

        return Stream.concat(programLevelRange,
                allCalls.stream().map(call -> {
                    TranspilerNode entryBlock = labelledBlock(((JumpTranspilerNode) call).getStart());
                    TranspilerNode exitBlock = labelledBlock(((JumpTranspilerNode) call).getEnd());
                    return ImmutablePair.of(
                            entryOrExitVertex(entryBlock, CodeSentinelType.ENTER),
                            entryOrExitVertex(exitBlock, CodeSentinelType.EXIT));
                })).collect(Collectors.toUnmodifiableSet());
    }

    private TranspilerNode labelledBlock(LocationNode locationNode) {
        return findAllRecursive(tree, n -> n instanceof LabelledTranspilerCodeBlockNode l && l.getName().equals(locationNode.name())).stream().findFirst().get();
    }

    private TranspilerInstruction entryOrExitVertex(TranspilerNode entryBlock, CodeSentinelType sentinelType) {
        return instructions.stream().filter(instr -> instr.ref() == entryBlock && instr.sentinel() == sentinelType).findFirst().get();
    }

    private Set<TranspilerNode> findAllRecursive(TranspilerNode current, Function<TranspilerNode, Boolean> match) {
        if (match.apply(current))
            return Stream.concat(Stream.of(current), childResults(current, match)).collect(Collectors.toUnmodifiableSet());
        return childResults(current, match).collect(Collectors.toUnmodifiableSet());
    }

    private List<TranspilerNode> findAllRecursiveOrdered(TranspilerNode current, Function<TranspilerNode, Boolean> match) {
        if (match.apply(current))
            return Stream.concat(Stream.of(current), childResults(current, match)).toList();
        return childResults(current, match).toList();
    }

    private Stream<TranspilerNode> childResults(TranspilerNode current, Function<TranspilerNode, Boolean> match) {
        return current.astChildren().stream().flatMap(c -> findAllRecursive(c, match).stream());
    }
}
