package org.smojol.toolkit.analysis.task.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.transpiler.*;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ProcedureBodyTask {
    private final TranspilerNode program;
    private final List<TranspilerInstruction> instructions;
    private final Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph;
    private final TranspilerNode mainNode;

    public ProcedureBodyTask(TranspilerNode program, List<TranspilerInstruction> instructions, Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph, TranspilerNode mainNode) {
        this.program = program;
        this.instructions = instructions;
        this.instructionFlowgraph = instructionFlowgraph;
        this.mainNode = mainNode;
    }

    public ProcedureBodyTask(TranspilerNode program, List<TranspilerInstruction> instructions, Graph<TranspilerInstruction, DefaultEdge> implicitCFG) {
        this(program, instructions, implicitCFG, new NullTranspilerNode());
    }

    public Set<InvokingProcedureRange> run() {
        Set<ProcedureRange> bodiesWithoutChildren = new CallRangesTask(program, instructions, mainNode).run().stream().map(range -> new RangeBodyTask(instructionFlowgraph).run(range)).collect(Collectors.toUnmodifiableSet());
        return rangesWithChildren(bodiesWithoutChildren);
    }

    // TODO: Refactor this!!!
    public Set<InvokingProcedureRange> rangesWithChildren(Set<ProcedureRange> procRanges) {
        return procRanges.stream()
                .map(procRange -> {
                            Set<ProcedureRange> childRanges = procRange.body().vertexSet().stream()
                                    .filter(v -> v.ref() instanceof JumpTranspilerNode j && j.getEnd() != LocationNode.NULL)
                                    .flatMap(jmp -> {
                                        JumpTranspilerNode jmpNode = (JumpTranspilerNode) jmp.ref();
                                        return procRanges.stream().filter(bwc2 -> ((LabelledTranspilerCodeBlockNode) bwc2.entry().ref()).getName().equals(jmpNode.getStart().name())
                                                && ((LabelledTranspilerCodeBlockNode) bwc2.exit().ref()).getName().equals(jmpNode.getEnd().name()));
                                    }).collect(Collectors.toUnmodifiableSet());
                            return new InvokingProcedureRange(procRange, childRanges);
                        }
                ).collect(Collectors.toUnmodifiableSet());
    }
}
