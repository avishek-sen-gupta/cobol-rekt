package com.mojo.algorithms.domain;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.IdProvider;

import java.util.Collection;

import static com.mojo.algorithms.domain.CodeSentinelType.*;


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
