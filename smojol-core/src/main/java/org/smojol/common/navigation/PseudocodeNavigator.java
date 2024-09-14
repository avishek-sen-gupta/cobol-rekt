package org.smojol.common.navigation;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.ast.InternalControlFlowNode;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.common.pseudocode.CodeSentinelType;

import java.util.List;
import java.util.function.Predicate;
import java.util.logging.Logger;

public class PseudocodeNavigator {
    private static final Logger LOGGER = Logger.getLogger(PseudocodeNavigator.class.getName());
    public List<PseudocodeInstruction> findAllByCondition(Predicate<PseudocodeInstruction> condition, List<PseudocodeInstruction> instructions) {
        return instructions.stream().filter(condition).toList();
    }

    public PseudocodeInstruction findSingleByCondition(Predicate<PseudocodeInstruction> condition, List<PseudocodeInstruction> instructions) {
        List<PseudocodeInstruction> matching = findAllByCondition(condition, instructions);
        return !matching.isEmpty() ? matching.getFirst() : PseudocodeInstruction.NULL;
    }

    public Pair<List<PseudocodeInstruction>, List<PseudocodeInstruction>> findCallTargets(PseudocodeInstruction from, int iptr, List<PseudocodeInstruction> instructions) {
        LOGGER.finer("Finding call targets of " + from.toString());
        if (from.getNode().type() == FlowNodeType.NEXT_SENTENCE) {
            int searchIndex = iptr;
            while (searchIndex < instructions.size()) {
                PseudocodeInstruction searchedInstruction = instructions.get(searchIndex);
                if (searchedInstruction.getNode().type() == FlowNodeType.SENTENCE && searchedInstruction.getSentinelType() == CodeSentinelType.ENTER)
                    return ImmutablePair.of(ImmutableList.of(searchedInstruction), ImmutableList.of());
                searchIndex++;
            }
            return ImmutablePair.of(ImmutableList.of(), ImmutableList.of());
        } else if (from.getNode().type() == FlowNodeType.PERFORM || from.getNode().type() == FlowNodeType.GOTO) {
            List<FlowNode> callTargets = ((InternalControlFlowNode) from.getNode()).callTargets();
            List<PseudocodeInstruction> targetInstructionEntries = findAllByCondition(i -> callTargets.contains(i.getNode()) && i.getSentinelType() == CodeSentinelType.ENTER, instructions);
            List<PseudocodeInstruction> targetInstructionExits = from.getNode().type() == FlowNodeType.GOTO ? ImmutableList.of() : findAllByCondition(i -> callTargets.contains(i.getNode()) && i.getSentinelType() == CodeSentinelType.EXIT, instructions);
            return ImmutablePair.of(targetInstructionEntries, targetInstructionExits);
//            return zip(targetInstructionEntries.stream(), targetInstructionExits.stream(), (entry, exit) -> (Pair<PseudocodeInstruction, PseudocodeInstruction>) ImmutablePair.of(entry, exit)).toList();
        } else if (from.getNode().type() == FlowNodeType.STOP) {
            PseudocodeInstruction programExit = findSingleByCondition(i -> i.getNode().type() == FlowNodeType.PROCEDURE_DIVISION_BODY && i.getSentinelType() == CodeSentinelType.EXIT, instructions);
            return ImmutablePair.of(ImmutableList.of(programExit), ImmutableList.of());
        }

        return ImmutablePair.of(ImmutableList.of(), ImmutableList.of());
    }
}
