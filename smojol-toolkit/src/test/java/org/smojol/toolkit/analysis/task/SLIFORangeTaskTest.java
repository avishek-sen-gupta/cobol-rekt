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

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.smojol.common.transpiler.TreeMatcher.*;

public class SLIFORangeTaskTest {
    @Test
    public void canDetermineSLIFORangeBaseCase() {
        JumpTranspilerNode jump_B1 = new JumpTranspilerNode(new NamedLocationNode("B1"), new NamedLocationNode("B1"));
        LabelledTranspilerCodeBlockNode a1 = new LabelledTranspilerCodeBlockNode("A1", ImmutableList.of(p(), p(), jump_B1, new JumpTranspilerNode(new ProgramTerminalLocationNode())), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode b1 = new LabelledTranspilerCodeBlockNode("B1", ImmutableList.of(p(), p()), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode c1 = new LabelledTranspilerCodeBlockNode("C1", ImmutableList.of(p(), p()), ImmutableMap.of());
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(a1, b1, c1));
        block_(
                labelledBlock_(
                        print_(),
                        print_(),
                        jump_(),
                        jump_()
                ),
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

        Set<InvokingProcedureRange> rangesWithChildren = new ProcedureBodyTask(program, instructions, implicitCFG).run();
        SLIFORangeCriterionTask slifoRangeCriterionTask = new SLIFORangeCriterionTask(rangesWithChildren);
        assertEquals(1, rangesWithChildren.size());
        InvokingProcedureRange rangeWithChildren = rangesWithChildren.stream().findFirst().get();
        ProcedureRange range = rangeWithChildren.range();
        Set<ProcedureRange> rangesTerminatingInRange = slifoRangeCriterionTask.rangesTerminatingIn(range);
        assertEquals(ImmutableSet.of(), rangesTerminatingInRange);
        assertEquals(ImmutableSet.of(), rangeWithChildren.invokedRanges());
        assertTrue(slifoRangeCriterionTask.isSLIFO(rangeWithChildren, ImmutableSet.of()));
    }

    @Test
    public void canDetermineSLIFORangesInductively() {
        JumpTranspilerNode jump_B1 = new JumpTranspilerNode(new NamedLocationNode("B1"), new NamedLocationNode("B1"));
        JumpTranspilerNode jump_C1 = new JumpTranspilerNode(new NamedLocationNode("C1"), new NamedLocationNode("C1"));
        LabelledTranspilerCodeBlockNode a1 = new LabelledTranspilerCodeBlockNode("A1", ImmutableList.of(p(), p(), jump_B1, new JumpTranspilerNode(new ProgramTerminalLocationNode())), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode b1 = new LabelledTranspilerCodeBlockNode("B1", ImmutableList.of(p(), p(), jump_C1), ImmutableMap.of());
        LabelledTranspilerCodeBlockNode c1 = new LabelledTranspilerCodeBlockNode("C1", ImmutableList.of(p(), p()), ImmutableMap.of());
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(a1, b1, c1));
        block_(
                labelledBlock_(
                        print_(),
                        print_(),
                        jump_(),
                        jump_()
                ),
                labelledBlock_(
                        print_(),
                        print_(),
                        jump_()
                ),
                labelledBlock_(
                        print_(),
                        print_()
                )
        ).verify(program);

        List<TranspilerInstruction> instructions = new BuildTranspilerInstructionsFromIntermediateTreeTask(program, new IncrementingIdProvider()).run();
        Graph<TranspilerInstruction, DefaultEdge> implicitCFG = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();
        Set<InvokingProcedureRange> rangesWithChildren = new ProcedureBodyTask(program, instructions, implicitCFG).run();
        SLIFORangeCriterionTask task = new SLIFORangeCriterionTask(rangesWithChildren);
        assertEquals(2, rangesWithChildren.size());
        Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> categorisedRanges = task.run();
        Set<InvokingProcedureRange> allSLIFORanges = categorisedRanges.getLeft();
        assertEquals(2, allSLIFORanges.size());
    }

    private static PrintTranspilerNode p() {
        return new PrintTranspilerNode(ImmutableList.of());
    }
}
