package org.smojol.common.id;

import org.smojol.common.pseudocode.CodeSentinelType;

public interface InstructionLike extends Identifiable {
    CodeSentinelType sentinel();
}
