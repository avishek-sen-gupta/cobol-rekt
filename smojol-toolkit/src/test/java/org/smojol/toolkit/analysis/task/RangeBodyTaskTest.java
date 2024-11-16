package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromRawASTTask;
import org.smojol.toolkit.analysis.task.transpiler.CallRangesTask;
import org.smojol.toolkit.analysis.task.transpiler.RangeBodyTask;

import java.util.List;

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
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = new BuildInstructionFlowgraphTask(instructions, program, ImmutableList.of()).run();

        Graph<TranspilerInstruction, DefaultEdge> body = new RangeBodyTask(instructionFlowgraph).run("A1", "A1");
        assertEquals(9, body.vertexSet().size());
    }

    private static PrintTranspilerNode c() {
        return new PrintTranspilerNode(ImmutableList.of());
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
