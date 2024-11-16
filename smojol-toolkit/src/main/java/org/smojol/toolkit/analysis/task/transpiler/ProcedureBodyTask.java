package org.smojol.toolkit.analysis.task.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.transpiler.ProcedureRange;
import org.smojol.common.transpiler.TranspilerCodeBlockNode;
import org.smojol.common.transpiler.TranspilerInstruction;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ProcedureBodyTask {
    public Set<ProcedureRange> run(TranspilerCodeBlockNode program, List<TranspilerInstruction> instructions, Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph) {
        Set<ProcedureRange> bodiesWithoutChildren = new CallRangesTask(program, instructions).run().stream().map(range -> new RangeBodyTask(instructionFlowgraph).run(range)).collect(Collectors.toUnmodifiableSet());
        return bodiesWithoutChildren;
    }


//    public Set<Triple<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>, >> invokedRanges(Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> bodiesWithoutChildren) {
//        bodiesWithoutChildren.stream().map(bwc -> bwc.getRight().vertexSet().stream()
//                .filter(v -> v.ref() instanceof JumpTranspilerNode j && j.getEnd() != LocationNode.NULL)
//                .flatMap(jmp -> {
//                    JumpTranspilerNode jmpNode = (JumpTranspilerNode) jmp.ref();
//                    return bodiesWithoutChildren.stream().filter(bwc2 -> ((LabelledTranspilerCodeBlockNode) bwc2.getLeft().getLeft().ref()).getName().equals(jmpNode.getStart().name())
//                            && ((LabelledTranspilerCodeBlockNode) bwc2.getLeft().getRight().ref()).getName().equals(jmpNode.getEnd().name()));
//                })
//                .map(childRange -> new ProcedureRange(bwc.getLeft().getLeft(), bwc.getLeft().getRight(), bwc.getRight(), invokedRanges()))
//        )
//    }
}
