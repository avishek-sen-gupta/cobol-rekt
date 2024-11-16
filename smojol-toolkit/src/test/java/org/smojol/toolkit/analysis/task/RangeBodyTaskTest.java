package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.transpiler.*;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.ProcedureBodyTask;
import org.smojol.toolkit.analysis.task.transpiler.RangeBodyTask;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.smojol.common.transpiler.TreeMatcher.*;

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

        Set<Pair<ProcedureRange, Set<ProcedureRange>>> rangesWithChildren = new ProcedureBodyTask(program, instructions, implicitCFG).run();
        assertEquals(2, rangesWithChildren.size());
        Pair<ProcedureRange, Set<ProcedureRange>> range33WithChildren = rangesWithChildren.stream().filter(rwc -> rwc.getLeft().body().vertexSet().size() == 33).findFirst().get();
        Pair<ProcedureRange, Set<ProcedureRange>> range21WithChildren = rangesWithChildren.stream().filter(rwc -> rwc.getLeft().body().vertexSet().size() == 21).findFirst().get();
        assertEquals(33, range33WithChildren.getLeft().body().vertexSet().size());
        assertEquals(21, range21WithChildren.getLeft().body().vertexSet().size());
        ProcedureRange range33 = range33WithChildren.getLeft();
        ProcedureRange range21 = range21WithChildren.getLeft();
        SLIFORangeCriterionTask slifoRangeCriterionTask = new SLIFORangeCriterionTask(rangesWithChildren);
        Set<ProcedureRange> rangesTerminatingInRange33 = slifoRangeCriterionTask.rangesTerminatingIn(range33);
        assertEquals(ImmutableSet.of(range21), rangesTerminatingInRange33);
        assertEquals(ImmutableSet.of(range33), range21WithChildren.getRight());
        assertEquals(ImmutableSet.of(range21, range33), range33WithChildren.getRight());
    }

    private static PrintTranspilerNode p() {
        return new PrintTranspilerNode(ImmutableList.of());
    }
}
