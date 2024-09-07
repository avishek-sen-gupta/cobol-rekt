package org.smojol.toolkit.ast;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.program.StaticCallTarget;
import org.smojol.common.pseudocode.PseudocodeInstruction;

import java.util.List;
import java.util.logging.Logger;

public class DynamicCallTarget extends CallTarget {
    private static final Logger LOGGER = Logger.getLogger(DynamicCallTarget.class.getName());
    private CobolParser.GeneralIdentifierContext cobolIdentifier;
    private IdmsParser.GeneralIdentifierContext idmsIdentifier;

    public DynamicCallTarget(CobolParser.GeneralIdentifierContext identifier) {
        super(identifier.getText(), ProgramReferenceType.DYNAMIC);
        this.cobolIdentifier = identifier;
    }

    public DynamicCallTarget(IdmsParser.GeneralIdentifierContext identifier) {
        super(identifier.getText(), ProgramReferenceType.DYNAMIC);
        idmsIdentifier = identifier;
    }

    // TODO: There may be a nicer way of doing this, but this dumb analysis is a good enough stand-in
    @Override
    public CallTarget resolve(PseudocodeInstruction instruction, List<PseudocodeInstruction> instructions) {
        int transferIndex = instructions.indexOf(instruction);
        String variableName = ((ExternalControlFlowNode) instruction.getNode()).callTarget().getName();
        int searchIndex = transferIndex - 1;
        while (searchIndex >= 0 && transferIndex - searchIndex <= 5) {
            PseudocodeInstruction candidateInstruction = instructions.get(searchIndex);
            if (candidateInstruction.getNode().type() != FlowNodeType.MOVE) {
                searchIndex--;
                continue;
            }
            List<CobolParser.GeneralIdentifierContext> tos = ((MoveFlowNode) candidateInstruction.getNode()).getTos();
            if (tos.isEmpty()) {
                searchIndex--;
                continue;
            }

            List<CobolParser.GeneralIdentifierContext> matchingMoves = tos.stream().filter(t -> t.qualifiedDataName() != null && t.qualifiedDataName().variableUsageName().getText().equals(variableName)).toList();
            if (matchingMoves.isEmpty()) {
                searchIndex--;
                continue;
            }

            CobolParser.MoveToSendingAreaContext froms = ((MoveFlowNode) candidateInstruction.getNode()).getFrom();
            if (froms.literal() == null) {
                searchIndex--;
                continue;
            }

            String resolvedTarget = froms.literal().getText();
            LOGGER.finer(String.format("Resolved a target: %s to %s", variableName, resolvedTarget));
            return new StaticCallTarget(resolvedTarget);
        }

        return this;
    }
}
