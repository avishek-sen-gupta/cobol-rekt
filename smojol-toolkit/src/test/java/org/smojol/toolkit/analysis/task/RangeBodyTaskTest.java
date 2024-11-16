package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.CallRangesTask;
import org.smojol.toolkit.analysis.task.transpiler.RangeBodyTask;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.smojol.common.transpiler.TreeMatcher.*;

public class RangeBodyTaskTest {
    @Test
    public void canCalculateRangeBody() {
        LabelledTranspilerCodeBlockNode a1 = new LabelledTranspilerCodeBlockNode("A1", ImmutableList.of(c(), c()), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode b1 = new LabelledTranspilerCodeBlockNode("B1", ImmutableList.of(c(), c()), ImmutableMap.of());
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(a1, b1));
        block_(
                labelledBlock_(
                        print_(),
                        print_()
                ),
                labelledBlock_(
                        print_(),
                        print_()
                )
        ).verify(program);

        List<TranspilerInstruction> instructions = new BuildTranspilerInstructionsFromIntermediateTreeTask(program, new IncrementingIdProvider()).run();
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();

        Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>> body = new RangeBodyTask(instructionFlowgraph).run("A1", "A1");
        assertEquals(9, body.getRight().vertexSet().size());
    }

    @Test
    public void canCalculateRangeBodyGivenRanges() {
        JumpTranspilerNode jump_A1_C1 = new JumpTranspilerNode(new NamedLocationNode("A1"), new NamedLocationNode("C1"));
        JumpTranspilerNode jump_A1_B1 = new JumpTranspilerNode(new NamedLocationNode("A1"), new NamedLocationNode("B1"));
        LabelledTranspilerCodeBlockNode a1 = new LabelledTranspilerCodeBlockNode("A1", ImmutableList.of(c(), c(), jump_A1_C1), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode b1 = new LabelledTranspilerCodeBlockNode("B1", ImmutableList.of(c(), c()), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode c1 = new LabelledTranspilerCodeBlockNode("C1", ImmutableList.of(c(), c(), jump_A1_B1), ImmutableMap.of());
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(a1, b1, c1));
        block_(
                labelledBlock_(
                        print_(),
                        print_(),
                        jump_()

                ),
                labelledBlock_(
                        print_(),
                        print_()
                ),
                labelledBlock_(
                        print_(),
                        print_(),
                        jump_()
                )
        ).verify(program);

        List<TranspilerInstruction> instructions = new BuildTranspilerInstructionsFromIntermediateTreeTask(program, new IncrementingIdProvider()).run();
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();

        Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> rangeBodies = new CallRangesTask(program, instructions).run().stream().map(range -> new RangeBodyTask(instructionFlowgraph).run(range)).collect(Collectors.toUnmodifiableSet());
        Set<Integer> graphVertexCardinalities = rangeBodies.stream().map(rangeBody -> rangeBody.getRight().vertexSet().size()).collect(Collectors.toUnmodifiableSet());
        assertEquals(ImmutableSet.of(21, 33), graphVertexCardinalities);
        Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>> range33 = rangeBodies.stream().filter(bbr -> bbr.getRight().vertexSet().size() == 33).findFirst().get();
        Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>> range21 = rangeBodies.stream().filter(bbr -> bbr.getRight().vertexSet().size() == 21).findFirst().get();
        Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> rangesTerminatingInCurrentRange = new SLIFO_RangeCriterionTask(rangeBodies).rangesTerminatingIn(range33);
        assertEquals(ImmutableSet.of(range21), rangesTerminatingInCurrentRange);
    }


    private static PrintTranspilerNode c() {
        return new PrintTranspilerNode(ImmutableList.of());
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
