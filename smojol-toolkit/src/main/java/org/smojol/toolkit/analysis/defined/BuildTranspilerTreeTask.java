package org.smojol.toolkit.analysis.defined;

import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.commons.lang3.tuple.Triple;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.ast.TranspilerInstructionGeneratorVisitor;
import org.smojol.common.navigation.AggregatingTranspilerNodeTraversal;
import org.smojol.common.pseudocode.*;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.task.*;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BuildTranspilerTreeTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;

    public BuildTranspilerTreeTask(FlowNode astRoot, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
    }

    @Override
    public AnalysisTaskResult run() {
//        TranspilerSetup.buildSymbolTable(astRoot, dataStructures, symbolTable);
        TranspilerNode transpilerTree = TranspilerTreeBuilder.flowToTranspiler(astRoot, dataStructures);
        TranspilerInstructionGeneratorVisitor visitor = new TranspilerInstructionGeneratorVisitor(new IncrementingIdProvider());
        new AggregatingTranspilerNodeTraversal<List<TranspilerInstruction>>().accept(transpilerTree, visitor);
        List<TranspilerInstruction> instructions = visitor.result();
        Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap = buildTranspilerNodeMap(instructions);
        List<TranspilerEdge> transpilerNodeEdges = controlFlowEdges(instructions, transpilerNodeMap);
        System.out.println(instructions);
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.ANALYSE_CONTROL_FLOW.name(), transpilerTree);
    }

    private List<TranspilerEdge> controlFlowEdges(List<TranspilerInstruction> instructions, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap) {
        List<TranspilerEdge> edges = new ArrayList<>();
        for (int i = 0; i < instructions.size() - 1; i++) {
            TranspilerInstruction currentInstruction = instructions.get(i);
            TranspilerInstruction nextInstruction = instructions.get(i + 1);
            TranspilerNode current = currentInstruction.ref();

            switch (current) {
                case IfTranspilerNode n when currentInstruction.sentinel() == CodeSentinelType.BODY -> {
                    TranspilerInstruction ifThenEntry = entry(n.getIfThenBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction ifElseEntry = entry(n.getIfElseBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction ifThenExit = exit(n.getIfThenBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction ifElseExit = exit(n.getIfElseBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction currentExit = exit(current, transpilerNodeMap, instructions);
                    edges.add(new TranspilerEdge(currentInstruction, ifThenEntry));
                    edges.add(new TranspilerEdge(currentInstruction, ifElseEntry));
                    edges.add(new TranspilerEdge(ifThenExit, currentExit));
                    edges.add(new TranspilerEdge(ifElseExit, currentExit));
                }
                case JumpTranspilerNode j when currentInstruction.sentinel() == CodeSentinelType.BODY -> {
                    TranspilerInstruction forwardTarget = entry(resolveNode(j.getStart(), instructions, i), transpilerNodeMap, instructions);
                    TranspilerInstruction returnCallSite = entry(resolveNode(j.getEnd(), instructions, i), transpilerNodeMap, instructions);
                    edges.add(new TranspilerEdge(body(current, transpilerNodeMap, instructions), forwardTarget));
                    if (returnCallSite.ref() instanceof NullTranspilerNode) continue;
                    edges.add(new TranspilerEdge(returnCallSite, exit(current, transpilerNodeMap, instructions)));
                }
                case TranspilerLoop transpilerLoop when currentInstruction.sentinel() == CodeSentinelType.EXIT ->
                        edges.add(new TranspilerEdge(currentInstruction, entry(current, transpilerNodeMap, instructions)));
                case ListIterationTranspilerNode listIterationTranspilerNode when currentInstruction.sentinel() == CodeSentinelType.EXIT ->
                        edges.add(new TranspilerEdge(currentInstruction, entry(current, transpilerNodeMap, instructions)));
                case null, default -> edges.add(new TranspilerEdge(currentInstruction, nextInstruction));
            }
        }

        return edges;
    }

    private TranspilerNode iterationExit(int currentAddress, List<TranspilerInstruction> instructions) {
        for (int searchAddress = currentAddress; searchAddress < instructions.size(); searchAddress++) {
            TranspilerInstruction instruction = instructions.get(searchAddress);
            if (instruction.ref() instanceof ListIterationTranspilerNode && instruction.sentinel() == CodeSentinelType.EXIT)
                return instruction.ref();
        }

        return new NullTranspilerNode();
    }

    private TranspilerInstruction entry(TranspilerNode node, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap, List<TranspilerInstruction> instructions) {
        return instructions.get(transpilerNodeMap.get(node).getLeft());
    }

    private TranspilerInstruction exit(TranspilerNode node, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap, List<TranspilerInstruction> instructions) {
        return instructions.get(transpilerNodeMap.get(node).getRight());
    }

    private TranspilerInstruction body(TranspilerNode node, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap, List<TranspilerInstruction> instructions) {
        return instructions.get(transpilerNodeMap.get(node).getMiddle());
    }

    private boolean isJump(TranspilerNode node) {
        return false;
    }

    private TranspilerNode resolveNode(LocationNode locationNode, List<TranspilerInstruction> instructions, int currentAddress) {
        return switch (locationNode) {
            case NamedLocationNode n ->
                    instructions.stream().filter(instr -> instr.ref() instanceof LabelledTranspilerCodeBlockNode && instr.sentinel() == CodeSentinelType.ENTER).findFirst().get().ref();
            case ProgramTerminalLocationNode n -> instructions.getLast().ref();
            case NextLocationNode n -> nextLocation(instructions, currentAddress);
            case ExitIterationScope s -> iterationExit(currentAddress, instructions);
            default -> new NullTranspilerNode();
        };
    }

    private static TranspilerNode nextLocation(List<TranspilerInstruction> instructions, int currentAddress) {
        for (int searchAddress = currentAddress; searchAddress < instructions.size(); searchAddress++) {
            if (!FlowNodeType.SENTENCE.equals(instructions.get(searchAddress).ref().getProperty("type")))
                continue;
            return instructions.get(searchAddress).ref();
        }
        return new NullTranspilerNode();
    }

    private static Map<TranspilerNode, Triple<Integer, Integer, Integer>> buildTranspilerNodeMap(List<TranspilerInstruction> instructions) {
        Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap = new HashMap<>();
        for (int i = 0; i < instructions.size(); i++) {
            TranspilerInstruction instruction = instructions.get(i);
            TranspilerNode node = instruction.ref();
            if (!transpilerNodeMap.containsKey(node)) transpilerNodeMap.put(node, ImmutableTriple.of(-1, -1, -1));
            if (instruction.sentinel() == CodeSentinelType.ENTER)
                transpilerNodeMap.put(node, ImmutableTriple.of(i, -1, -1));
            else if (instruction.sentinel() == CodeSentinelType.BODY)
                transpilerNodeMap.put(node, ImmutableTriple.of(transpilerNodeMap.get(node).getLeft(), i, -1));
            else if (instruction.sentinel() == CodeSentinelType.EXIT)
                transpilerNodeMap.put(node, ImmutableTriple.of(transpilerNodeMap.get(node).getLeft(), transpilerNodeMap.get(node).getMiddle(), i));
        }
        return transpilerNodeMap;
    }
}
