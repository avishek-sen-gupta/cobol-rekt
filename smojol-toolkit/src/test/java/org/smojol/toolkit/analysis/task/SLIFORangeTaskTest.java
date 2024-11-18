package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.ProcedureBodyTask;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.smojol.common.transpiler.TreeMatcher.*;

/**
 * From the paper 'Identifying Procedural Structure in Cobol Programs' by John Field and G.Ramalingam
 */
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

    @Test
    public void canDetermineNonSLIFORangesInductively() {
        LabelledTranspilerCodeBlockNode me = section("ME", p());
        LabelledTranspilerCodeBlockNode pa = section("PA", jmp("AE", "AX"));
        LabelledTranspilerCodeBlockNode pe = section("PE", jmp("EE", "EX"));
        LabelledTranspilerCodeBlockNode pb = section("PB", jmp("BE", "BX"));
        LabelledTranspilerCodeBlockNode mx = section("MX", new JumpTranspilerNode(new ProgramTerminalLocationNode()));
        LabelledTranspilerCodeBlockNode ae = section("AE", p());
        LabelledTranspilerCodeBlockNode pc = section("PC", jmp("CE", "CX"));
        LabelledTranspilerCodeBlockNode be = section("BE", p());
        LabelledTranspilerCodeBlockNode ce = section("CE", p());
        LabelledTranspilerCodeBlockNode cx = section("CX", p());
        LabelledTranspilerCodeBlockNode ax = section("AX", p());
        LabelledTranspilerCodeBlockNode d1 = section("D1", p());
        LabelledTranspilerCodeBlockNode d2 = section("D2", p());
        LabelledTranspilerCodeBlockNode ee = section("EE", new IfTranspilerNode(new PrimitiveValueTranspilerNode(TypedRecord.TRUE), new TranspilerCodeBlockNode(jmp("PB"))));
        LabelledTranspilerCodeBlockNode ex = section("EX", p());
        LabelledTranspilerCodeBlockNode bx = section("BX", p());

        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(
                me,
                pa,
                pe,
                pb,
                mx,
                ae,
                pc,
                be,
                ce,
                cx,
                ax,
                d1,
                d2,
                ee,
                ex,
                bx
        ));
        block_(
                labelledBlock_("ME", print_()),
                labelledBlock_("PA", jump_()),
                labelledBlock_("PE", jump_()),
                labelledBlock_("PB", jump_()),
                labelledBlock_("MX", jump_()),
                labelledBlock_("AE", print_()),
                labelledBlock_("PC", jump_()),
                labelledBlock_("BE", print_()),
                labelledBlock_("CE", print_()),
                labelledBlock_("CX", print_()),
                labelledBlock_("AX", print_()),
                labelledBlock_("D1", print_()),
                labelledBlock_("D2", print_()),
                labelledBlock_("EE", if_(
                        block_(jump_()),
                        any_()
                )),
                labelledBlock_("EX", print_()),
                labelledBlock_("BX", print_())
        ).verify(program);

        List<TranspilerInstruction> instructions = new BuildTranspilerInstructionsFromIntermediateTreeTask(program, new IncrementingIdProvider()).run();
        Graph<TranspilerInstruction, DefaultEdge> implicitCFG = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();
        Set<InvokingProcedureRange> rangesWithChildren = new ProcedureBodyTask(program, instructions, implicitCFG).run();
        SLIFORangeCriterionTask task = new SLIFORangeCriterionTask(rangesWithChildren);
        assertEquals(4, rangesWithChildren.size());
        Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> categorisedRanges = task.run();
        Set<InvokingProcedureRange> allSLIFORanges = categorisedRanges.getLeft();
        Set<InvokingProcedureRange> allNonSLIFORanges = categorisedRanges.getRight();
        assertEquals(2, allSLIFORanges.size());
        assertEquals(2, allNonSLIFORanges.size());
        Set<Pair<TranspilerNode, TranspilerNode>> allSLIFOProcRanges = allSLIFORanges.stream().map(invokingProcedureRange ->
                ImmutablePair.of(invokingProcedureRange.range().entry().ref(), invokingProcedureRange.range().exit().ref())).collect(Collectors.toUnmodifiableSet());
        Set<Pair<TranspilerNode, TranspilerNode>> allNonSLIFOProcRanges = allNonSLIFORanges.stream().map(invokingProcedureRange ->
                ImmutablePair.of(invokingProcedureRange.range().entry().ref(), invokingProcedureRange.range().exit().ref())).collect(Collectors.toUnmodifiableSet());
        assertEquals(ImmutableSet.of(ImmutablePair.of(ce, cx), ImmutablePair.of(ae, ax)), allSLIFOProcRanges);
        assertEquals(ImmutableSet.of(ImmutablePair.of(ee, ex), ImmutablePair.of(be, bx)), allNonSLIFOProcRanges);
    }

    private static LabelledTranspilerCodeBlockNode section(String sectionName, TranspilerNode... children) {
        return new LabelledTranspilerCodeBlockNode(sectionName, Arrays.asList(children), ImmutableMap.of("type", FlowNodeType.SECTION));
    }

    private static PrintTranspilerNode p() {
        return new PrintTranspilerNode(ImmutableList.of());
    }

    private static JumpTranspilerNode jmp(String from, String to) {
        return new JumpTranspilerNode(new NamedLocationNode(from), new NamedLocationNode(to));
    }

    private static JumpTranspilerNode jmp(String from) {
        return new JumpTranspilerNode(new NamedLocationNode(from), LocationNode.NULL);
    }
}
