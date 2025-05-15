package com.mojo.algorithms.task;

import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.domain.*;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.commons.lang3.tuple.Triple;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Logger;

public class BuildInstructionFlowgraphTask {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(BuildInstructionFlowgraphTask.class.getName());
    private final List<TranspilerInstruction> instructions;
    private final Map<TranspilerNode, Triple<Integer, Integer, Integer>> transpilerNodeMap;
    private final List<String> flowHints;
    private final List<TranspilerInstructionEdge> edges;

    public BuildInstructionFlowgraphTask(List<TranspilerInstruction> instructions, List<String> flowHints) {
        this.instructions = instructions;
        transpilerNodeMap = buildTranspilerNodeMap(instructions);
        this.flowHints = flowHints;
        edges = new ArrayList<>();
    }

    public Graph<TranspilerInstruction, DefaultEdge> run() {
        List<TranspilerInstructionEdge> instructionEdges = controlFlowEdges();
        Graph<TranspilerInstruction, DefaultEdge> jgraph = new DefaultDirectedGraph<>(DefaultEdge.class);
        instructions.forEach(jgraph::addVertex);
        instructionEdges.forEach(edge -> jgraph.addEdge(edge.from(), edge.to(), new AnnotatedEdge(edge.metadata())));
        return jgraph;
    }

    private List<TranspilerInstructionEdge> controlFlowEdges() {
        for (int i = 0; i < instructions.size() - 1; i++) {
            TranspilerInstruction currentInstruction = instructions.get(i);
            TranspilerInstruction nextInstruction = instructions.get(i + 1);
            TranspilerNode current = currentInstruction.ref();

            if (nextInstruction.ref() instanceof LabelledTranspilerCodeBlockNode b
                    && flowHints.contains(b.getName())
                    && (FlowNodeType.SECTION == b.getProperty("type") || FlowNodeType.PARAGRAPH == b.getProperty("type"))
                    && nextInstruction.sentinel() == CodeSentinelType.ENTER) {
                LOGGER.info("Skipping flow hint: " + b.getName());
                continue;
            }
            switch (current) {
                case IfTranspilerNode n when currentInstruction.sentinel() == CodeSentinelType.BODY -> {
                    TranspilerInstruction ifThenEntry = entry(n.getIfThenBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction ifElseEntry = entry(n.getIfElseBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction ifThenExit = exit(n.getIfThenBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction ifElseExit = exit(n.getIfElseBlock(), transpilerNodeMap, instructions);
                    TranspilerInstruction currentExit = exit(current, transpilerNodeMap, instructions);
                    addEdge(currentInstruction, ifThenEntry, "THEN_ENTRY");
                    addEdge(currentInstruction, ifElseEntry, "ELSE_ENTRY");
                    addEdge(ifThenExit, currentExit, "THEN_EXIT");
                    addEdge(ifElseExit, currentExit, "ELSE_EXIT");
                }
                case JumpTranspilerNode j when currentInstruction.sentinel() == CodeSentinelType.BODY -> {
                    TranspilerInstruction forwardTarget = resolveNode(j.getStart(), instructions, i);
                    TranspilerInstruction returnCallSite = exit(resolveNode(j.getEnd(), instructions, i).ref(), transpilerNodeMap, instructions);
                    addEdge(body(current, transpilerNodeMap, instructions), forwardTarget, "JUMP_ENTRY");
                    addEdge(returnCallSite, exit(current, transpilerNodeMap, instructions), "JUMP_RETURN");
                }
                case TranspilerLoop transpilerLoop when currentInstruction.sentinel() == CodeSentinelType.EXIT -> {
                    addEdge(currentInstruction, entry(current, transpilerNodeMap, instructions), "LOOPBACK");
                    addEdge(currentInstruction, nextInstruction, "ELSE_EXIT");
                }
                case ListIterationTranspilerNode listIterationTranspilerNode when currentInstruction.sentinel() == CodeSentinelType.EXIT -> {
                    addEdge(currentInstruction, entry(current, transpilerNodeMap, instructions), "LOOPBACK");
                    addEdge(currentInstruction, nextInstruction, "SEQUENTIAL");
                }
                case DetachedTranspilerCodeBlockNode x when currentInstruction.sentinel() == CodeSentinelType.EXIT -> {
                    // Don't do anything
                }
                default -> {
                    LOGGER.finer("Unknown instruction: " + currentInstruction.ref());
                    addEdge(currentInstruction, nextInstruction, "SEQUENTIAL");
                }
            }
        }

        return edges;
    }

    private void addEdge(TranspilerInstruction from, TranspilerInstruction to, String tag) {
        if (from == TranspilerInstruction.NULL || to == TranspilerInstruction.NULL) return;
        edges.add(new TranspilerInstructionEdge(from, to, ImmutableMap.of("edgeType", tag)));
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

    private TranspilerInstruction resolveNode(LocationNode locationNode, List<TranspilerInstruction> instructions, int currentAddress) {
        return switch (locationNode) {
            case NamedLocationNode n ->
                    entry(instructions.stream().filter(instr -> instr.ref() instanceof LabelledTranspilerCodeBlockNode x && instr.sentinel() == CodeSentinelType.ENTER && x.getName().equals(n.getName())).findFirst().get().ref(), transpilerNodeMap, instructions);
            case ProgramTerminalLocationNode n -> exit(instructions.getLast().ref(), transpilerNodeMap, instructions);
            case NextLocationNode n ->
                    entry(nextLocation(instructions, currentAddress), transpilerNodeMap, instructions);
            case ExitIterationScopeLocationNode s ->
                    exit(iterationExit(currentAddress, instructions), transpilerNodeMap, instructions);
            default -> TranspilerInstruction.NULL;
        };
    }

    private static TranspilerNode nextLocation(List<TranspilerInstruction> instructions, int currentAddress) {
        for (int searchAddress = currentAddress; searchAddress < instructions.size(); searchAddress++) {
            if (FlowNodeType.SENTENCE.equals(instructions.get(searchAddress).ref().getProperty("type"))
                    && instructions.get(searchAddress).sentinel() == CodeSentinelType.ENTER)
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
