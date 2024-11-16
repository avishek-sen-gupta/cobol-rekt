package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.ProcedureBodyTask;
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

        ProcedureRange range = new RangeBodyTask(instructionFlowgraph).run("A1", "A1");
        assertEquals(9, range.body().vertexSet().size());
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

        Set<ProcedureRange> procedureRanges = new ProcedureBodyTask().run(program, instructions, instructionFlowgraph);
        Set<Integer> graphVertexCardinalities = procedureRanges.stream().map(procRange -> procRange.body().vertexSet().size()).collect(Collectors.toUnmodifiableSet());
        assertEquals(ImmutableSet.of(21, 33), graphVertexCardinalities);
        ProcedureRange range33 = procedureRanges.stream().filter(bbr -> bbr.body().vertexSet().size() == 33).findFirst().get();
        ProcedureRange range21 = procedureRanges.stream().filter(bbr -> bbr.body().vertexSet().size() == 21).findFirst().get();
        SLIFORangeCriterionTask slifoRangeCriterionTask = new SLIFORangeCriterionTask(procedureRanges);
        Set<ProcedureRange> rangesTerminatingInCurrentRange = slifoRangeCriterionTask.rangesTerminatingIn(range33);
//        Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> invokedRanges = slifoRangeCriterionTask.invokedRanges(range33);
        assertEquals(ImmutableSet.of(range21), rangesTerminatingInCurrentRange);
    }



    private static PrintTranspilerNode c() {
        return new PrintTranspilerNode(ImmutableList.of());
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
