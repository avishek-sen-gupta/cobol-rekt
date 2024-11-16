package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.transpiler.*;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.CallRangesTask;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.smojol.common.transpiler.TreeMatcher.*;

public class CallRangesTaskTest {
    @Test
    public void canCalculateRangeBody() {
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
        Set<Pair<TranspilerInstruction, TranspilerInstruction>> ranges = new CallRangesTask(program, instructions).run();

        assertEquals(2, ranges.size());
    }

    private static PrintTranspilerNode c() {
        return new PrintTranspilerNode(ImmutableList.of());
    }
}
