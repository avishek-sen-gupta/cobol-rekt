package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.id.IdProvider;

import java.util.Collection;

import static org.smojol.common.pseudocode.CodeSentinelType.*;

public class TranspilerInstructionGenerator {
    public static Collection<TranspilerInstruction> body(TranspilerNode node, IdProvider uuidProvider) {
        return ImmutableList.of(new TranspilerInstruction(node, BODY, uuidProvider.next()));
    }

    public static TranspilerInstruction entering(TranspilerNode node, IdProvider uuidProvider) {
        return new TranspilerInstruction(node, ENTER, uuidProvider.next());
    }

    public static TranspilerInstruction exiting(TranspilerNode node, IdProvider uuidProvider) {
        return new TranspilerInstruction(node, EXIT, uuidProvider.next());
    }
}
