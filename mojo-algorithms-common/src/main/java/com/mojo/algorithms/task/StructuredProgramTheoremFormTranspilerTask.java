package com.mojo.algorithms.task;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.domain.TranspilerFlowgraph;
import com.mojo.algorithms.domain.TranspilerInstruction;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.transpiler.*;
import org.jgrapht.graph.DefaultEdge;
import com.mojo.algorithms.domain.BasicBlock;

import java.util.ArrayList;
import java.util.List;

public class StructuredProgramTheoremFormTranspilerTask {

    private final TranspilerFlowgraph transpilerFlowgraph;

    public StructuredProgramTheoremFormTranspilerTask(TranspilerFlowgraph transpilerFlowgraph) {
        this.transpilerFlowgraph = transpilerFlowgraph;
    }

    public List<String> run() {
        List<BasicBlock<TranspilerInstruction>> basicBlocks = transpilerFlowgraph.basicBlocks();
        basicBlocks.forEach(basicBlock -> System.out.println(basicBlock.lastInstruction()));
        return basicBlocks.stream().flatMap(b -> transpiled(b).stream()).toList();
    }

    private List<String> transpiled(BasicBlock<TranspilerInstruction> basicBlock) {
        List<String> allInstructions = new ArrayList<>();
        allInstructions.add(String.format("case('%s') [START]------------------------------------->>>", basicBlock.id()));
        allInstructions.addAll(basicBlock.getInstructions().stream().flatMap(instr -> transpiled(instr, basicBlock).stream()).toList());
        allInstructions.add(String.format("case('%s') [END]<<<-------------------------------------", basicBlock.id()));
        return allInstructions;
    }

    private List<String> transpiled(TranspilerInstruction instruction, BasicBlock<TranspilerInstruction> basicBlock) {
        if (instruction.sentinel() == CodeSentinelType.ENTER) return ImmutableList.of();
        else if (instruction.sentinel() == CodeSentinelType.EXIT) return ImmutableList.of();
        else if (instruction.ref() instanceof JumpTranspilerNode j) {
            DefaultEdge jumpEdge = transpilerFlowgraph.basicBlockFlowgraph().outgoingEdgesOf(basicBlock).stream().findFirst().get();
            BasicBlock<TranspilerInstruction> jumpBasicBlock = transpilerFlowgraph.basicBlockFlowgraph().getEdgeTarget(jumpEdge);
            return ImmutableList.of(String.format("[%s] nextBlock=%s; continue;", basicBlock.id(), jumpBasicBlock.id()));
        } else if (instruction.ref() instanceof IfTranspilerNode i) {
            return ImmutableList.of(String.format("[%s] if (%s)", basicBlock.id(), i.getCondition().description()),
                    "then {nextBlock=" + blockContaining(i.getIfThenBlock()).id() + "}; continue;",
                    "else nextBlock=" + blockContaining(i.getIfElseBlock()).id() + "}; continue;"
            );
        } else if (instruction.ref() instanceof LabelledTranspilerCodeBlockNode
                || instruction.ref() instanceof TranspilerCodeBlockNode)
            return ImmutableList.of(String.format("[%s] --------", basicBlock.id()));
        else return ImmutableList.of(String.format("[%s] %s", basicBlock.id(), instruction.originalText()));
    }

    private BasicBlock<TranspilerInstruction> blockContaining(TranspilerNode instruction) {
        return transpilerFlowgraph.basicBlocks().stream().filter(bb -> contains(instruction, CodeSentinelType.ENTER, bb)).findFirst().get();
    }

    private boolean contains(TranspilerNode node, CodeSentinelType codeSentinelType, BasicBlock<TranspilerInstruction> bb) {
        return bb.getInstructions().stream().anyMatch(instr -> instr.ref() == node && instr.sentinel() == codeSentinelType);
    }
}
