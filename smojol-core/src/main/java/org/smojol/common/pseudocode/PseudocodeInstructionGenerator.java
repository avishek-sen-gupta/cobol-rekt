package org.smojol.common.pseudocode;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;

import static org.smojol.common.pseudocode.PseudocodeMetatype.*;

public class PseudocodeInstructionGenerator {
    public static PseudocodeInstruction visiting(FlowNode node) {
        return isMarker(node) ? new PseudocodeInstruction(node, NO_OP) : new PseudocodeInstruction(node, BODY);
    }

    public static PseudocodeInstruction entering(FlowNode node) {
        return isMarker(node) ? new PseudocodeInstruction(node, NO_OP) : new PseudocodeInstruction(node, ENTER);
    }

    public static PseudocodeInstruction exiting(FlowNode node) {
        return isMarker(node) ? new PseudocodeInstruction(node, NO_OP) : new PseudocodeInstruction(node, EXIT);
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
