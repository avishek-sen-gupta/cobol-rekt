package org.smojol.common.pseudocode;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
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

    private static boolean isMarker(FlowNode node) {
        return node.type() == FlowNodeType.PARAGRAPH
                || node.type() == FlowNodeType.PARAGRAPH_NAME
                || node.type() == FlowNodeType.PARAGRAPHS
                || node.type() == FlowNodeType.SENTENCE
                || node.type() == FlowNodeType.COMPOSITE
                || node.type() == FlowNodeType.PROCEDURE_DIVISION_BODY
                || node.type() == FlowNodeType.SECTION_HEADER
                || node.type() == FlowNodeType.SECTION
                || node.type() == FlowNodeType.CONDITION_CLAUSE;
    }
}
