package org.smojol.common.transpiler;

import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.commons.lang3.tuple.Triple;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.pseudocode.CodeSentinelType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class TranspilerModelBuilder {
    private final List<TranspilerInstruction> instructions;
    private final TranspilerNode transpilerTree;

    public TranspilerModelBuilder(List<TranspilerInstruction> instructions, TranspilerNode transpilerTree) {
        this.instructions = instructions;
        this.transpilerTree = transpilerTree;
    }

    public TranspilerModel build() {
        Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap = buildTranspilerNodeMap(instructions);
        List<TranspilerEdge> instructionEdges = controlFlowEdges(instructions, transpilerNodeMap);
        return new TranspilerModel(transpilerTree, instructions, instructionEdges);
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
                    TranspilerInstruction forwardTarget = j.getStart() instanceof ExitIterationScopeLocationNode
                            ? exit(resolveNode(j.getStart(), instructions, i), transpilerNodeMap, instructions)
                            : entry(resolveNode(j.getStart(), instructions, i), transpilerNodeMap, instructions);
                    TranspilerInstruction returnCallSite = exit(resolveNode(j.getEnd(), instructions, i), transpilerNodeMap, instructions);
                    edges.add(new TranspilerEdge(body(current, transpilerNodeMap, instructions), forwardTarget));
                    if (returnCallSite == TranspilerInstruction.NULL) continue;
                    edges.add(new TranspilerEdge(returnCallSite, exit(current, transpilerNodeMap, instructions)));
                }
                case TranspilerLoop transpilerLoop when currentInstruction.sentinel() == CodeSentinelType.EXIT -> {
                    edges.add(new TranspilerEdge(currentInstruction, body(current, transpilerNodeMap, instructions)));
                    edges.add(new TranspilerEdge(currentInstruction, nextInstruction));
                }
                case ListIterationTranspilerNode listIterationTranspilerNode when currentInstruction.sentinel() == CodeSentinelType.EXIT -> {
                    edges.add(new TranspilerEdge(currentInstruction, body(current, transpilerNodeMap, instructions)));
                    edges.add(new TranspilerEdge(currentInstruction, nextInstruction));
                }
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
        return instructionMarkedAs(Triple::getLeft, node, transpilerNodeMap, instructions);
    }

    private TranspilerInstruction exit(TranspilerNode node, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap, List<TranspilerInstruction> instructions) {
        return instructionMarkedAs(Triple::getRight, node, transpilerNodeMap, instructions);
    }

    private TranspilerInstruction body(TranspilerNode node, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap, List<TranspilerInstruction> instructions) {
        return instructionMarkedAs(Triple::getMiddle, node, transpilerNodeMap, instructions);
    }

    private static TranspilerInstruction instructionMarkedAs(Function<Triple<Integer, Integer, Integer>, Integer> access, TranspilerNode node, Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap, List<TranspilerInstruction> instructions) {
        if (node instanceof NullTranspilerNode) return TranspilerInstruction.NULL;
        return instructions.get(access.apply(transpilerNodeMap.get(node)));
    }

    private TranspilerNode resolveNode(LocationNode locationNode, List<TranspilerInstruction> instructions, int currentAddress) {
        return switch (locationNode) {
            case NamedLocationNode n ->
                    instructions.stream().filter(instr -> instr.ref() instanceof LabelledTranspilerCodeBlockNode && instr.sentinel() == CodeSentinelType.ENTER && ((LabelledTranspilerCodeBlockNode) instr.ref()).getName().equals(n.getName())).findFirst().get().ref();
            case ProgramTerminalLocationNode n -> instructions.getLast().ref();
            case NextLocationNode n -> nextLocation(instructions, currentAddress);
            case ExitIterationScopeLocationNode s -> iterationExit(currentAddress, instructions);
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
