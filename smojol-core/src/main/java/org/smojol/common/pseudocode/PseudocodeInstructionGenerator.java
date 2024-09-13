package org.smojol.common.pseudocode;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.IdProvider;

import java.util.List;

import static org.smojol.common.pseudocode.CodeSentinelType.*;

public class PseudocodeInstructionGenerator {

    public static List<PseudocodeInstruction> visiting(FlowNode node, IdProvider uuidProvider) {
        return ImmutableList.of(new PseudocodeInstruction(node, BODY, uuidProvider.next()));
    }

    public static PseudocodeInstruction entering(FlowNode node, IdProvider uuidProvider) {
        return new PseudocodeInstruction(node, ENTER, uuidProvider.next());
    }

    public static PseudocodeInstruction exiting(FlowNode node, IdProvider uuidProvider) {
        return new PseudocodeInstruction(node, EXIT, uuidProvider.next());
    }
}
