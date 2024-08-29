package org.smojol.common.navigation;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.ast.InternalControlFlowNode;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.common.pseudocode.PseudocodeMetatype;

import java.util.List;
import java.util.function.Predicate;

import static com.google.common.collect.Streams.zip;

public class PseudocodeNavigator {
    private final FlowNode root;

    public PseudocodeNavigator(FlowNode root) {
        this.root = root;
    }

    public List<PseudocodeInstruction> findAllByCondition(Predicate<PseudocodeInstruction> condition, List<PseudocodeInstruction> instructions) {
        return instructions.stream().filter(condition).toList();
    }

    public PseudocodeInstruction findSingleByCondition(Predicate<PseudocodeInstruction> condition, List<PseudocodeInstruction> instructions) {
        List<PseudocodeInstruction> mathing = findAllByCondition(condition, instructions);
        return !mathing.isEmpty() ? mathing.getFirst() : PseudocodeInstruction.NULL;
    }

    public List<ImmutablePair<PseudocodeInstruction, PseudocodeInstruction>> findCallTargets(PseudocodeInstruction from, int iptr, List<PseudocodeInstruction> instructions) {
        System.out.println("Finding call targets of " + from.toString());
        if (from.getNode().type() == FlowNodeType.NEXT_SENTENCE) {
            int searchIndex = iptr;
            while (searchIndex < instructions.size()) {
                PseudocodeInstruction searchedInstruction = instructions.get(searchIndex);
                if (searchedInstruction.getNode().type() == FlowNodeType.SENTENCE && searchedInstruction.getMetatype() == PseudocodeMetatype.ENTER)
                    return ImmutableList.of(ImmutablePair.of(searchedInstruction, PseudocodeInstruction.NULL));
                searchIndex++;
            }
            return ImmutableList.of();
        } else if (from.getNode().type() == FlowNodeType.PERFORM || from.getNode().type() == FlowNodeType.GOTO) {
            List<FlowNode> callTargets = ((InternalControlFlowNode) from.getNode()).callTargets();
            List<PseudocodeInstruction> targetInstructionEntries = findAllByCondition(i -> callTargets.contains(i.getNode()) && i.getMetatype() == PseudocodeMetatype.ENTER, instructions);
            List<PseudocodeInstruction> targetInstructionExits = from.getNode().type() == FlowNodeType.GOTO ? ImmutableList.of() : findAllByCondition(i -> callTargets.contains(i.getNode()) && i.getMetatype() == PseudocodeMetatype.EXIT, instructions);
            return zip(targetInstructionEntries.stream(), targetInstructionExits.stream(), (entry, exit) -> ImmutablePair.of(entry, exit)).toList();
        }

        return ImmutableList.of();
    }
}
