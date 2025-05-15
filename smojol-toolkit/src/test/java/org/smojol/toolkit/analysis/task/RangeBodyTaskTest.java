package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.mojo.algorithms.id.IncrementingIdProvider;
import com.mojo.algorithms.task.RangeBodyTask;
import com.mojo.algorithms.transpiler.*;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;

import java.util.List;
import java.util.Set;

import static com.mojo.algorithms.transpiler.TreeMatcher.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class RangeBodyTaskTest {
    @Test
    public void canCalculateRangeBody() {
        LabelledTranspilerCodeBlockNode a1 = new LabelledTranspilerCodeBlockNode("A1", ImmutableList.of(p(), p()), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode b1 = new LabelledTranspilerCodeBlockNode("B1", ImmutableList.of(p(), p()), ImmutableMap.of());
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
        Graph<TranspilerInstruction, DefaultEdge> implicitCFG = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();

        ProcedureRange range = new RangeBodyTask(implicitCFG).run("A1", "A1");
        assertEquals(9, range.body().vertexSet().size());
    }

    @Test
    public void canCalculateRangeBodyGivenRanges() {
        JumpTranspilerNode jump_A1_C1 = new JumpTranspilerNode(new NamedLocationNode("A1"), new NamedLocationNode("C1"));
        JumpTranspilerNode jump_A1_B1 = new JumpTranspilerNode(new NamedLocationNode("A1"), new NamedLocationNode("B1"));
        LabelledTranspilerCodeBlockNode a1 = new LabelledTranspilerCodeBlockNode("A1", ImmutableList.of(p(), p(), jump_A1_C1), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode b1 = new LabelledTranspilerCodeBlockNode("B1", ImmutableList.of(p(), p()), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode c1 = new LabelledTranspilerCodeBlockNode("C1", ImmutableList.of(p(), p(), jump_A1_B1), ImmutableMap.of());
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
        Graph<TranspilerInstruction, DefaultEdge> implicitCFG = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();

        Set<InvokingProcedureRange> rangesWithChildren = new ProcedureBodyTask(program, instructions, implicitCFG).run();
        assertEquals(2, rangesWithChildren.size());
        InvokingProcedureRange range33WithChildren = rangesWithChildren.stream().filter(rwc -> rwc.range().body().vertexSet().size() == 33).findFirst().get();
        InvokingProcedureRange range21WithChildren = rangesWithChildren.stream().filter(rwc -> rwc.range().body().vertexSet().size() == 21).findFirst().get();
        assertEquals(33, range33WithChildren.range().body().vertexSet().size());
        assertEquals(21, range21WithChildren.range().body().vertexSet().size());
        ProcedureRange range33 = range33WithChildren.range();
        ProcedureRange range21 = range21WithChildren.range();
        SLIFORangeCriterionTask slifoRangeCriterionTask = new SLIFORangeCriterionTask(rangesWithChildren);
        Set<ProcedureRange> rangesTerminatingInRange33 = slifoRangeCriterionTask.rangesTerminatingIn(range33);
        assertEquals(ImmutableSet.of(range21), rangesTerminatingInRange33);
        assertEquals(ImmutableSet.of(range33), range21WithChildren.invokedRanges());
        assertEquals(ImmutableSet.of(range21, range33), range33WithChildren.invokedRanges());
    }

    private static PrintTranspilerNode p() {
        return new PrintTranspilerNode(ImmutableList.of());
    }
}
