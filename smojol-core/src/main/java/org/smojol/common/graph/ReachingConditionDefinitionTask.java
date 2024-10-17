package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.smojol.common.pseudocode.CodeSentinelType.BODY;


public class ReachingConditionDefinitionTask<V extends TranspilerInstruction, E> {
    private final GraphSlice<V, E> slice;

    public ReachingConditionDefinitionTask(GraphSlice<V, E> slice) {
        this.slice = slice;
    }

    public List<Pair<V, TranspilerNode>> run() {
        Stream<Pair<V, TranspilerNode>> reachingConditionAllPaths = slice.allPaths().stream().flatMap(path -> IntStream.range(0, path.getVertexList().size()).mapToObj(i -> (Pair<V, List<V>>) ImmutablePair.of(path.getVertexList().get(i), path.getVertexList().subList(0, i))).toList().stream().map(rc -> (Pair<V, TranspilerNode>) ImmutablePair.of(rc.getLeft(), rc.getRight().stream()
                .filter(instr -> instr.sentinel() == BODY && instr.ref() instanceof IfTranspilerNode)
                .map(TranspilerInstruction::ref)
                .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE),
                        AndTranspilerNode::new))));
        List<Pair<V, TranspilerNode>> exhaustiveReachingConditions = reachingConditionAllPaths
                .collect(Collectors.groupingBy(Pair::getLeft)).entrySet().stream()
                .map(p -> ImmutablePair.of(p.getKey(), p.getValue().stream()
                        .map(Pair::getRight)))
                .map(p1 -> (Pair<V, TranspilerNode>) ImmutablePair.of(p1.getLeft(), p1.getRight()
                        .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE), OrTranspilerNode::new))).toList();
        return exhaustiveReachingConditions;
//        return slice.allPaths().stream().map(path -> path.getVertexList().stream().filter(v -> v.sentinel() == BODY && v.ref() instanceof IfTranspilerNode)
//                .map(obj -> ((IfTranspilerNode) obj.ref()).getCondition())
//                .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE), AndTranspilerNode::new)).collect(Collectors.toUnmodifiableSet());
    }
}
