package com.mojo.algorithms.id;


import com.mojo.algorithms.domain.CodeSentinelType;

public interface InstructionLike extends Identifiable {
    CodeSentinelType sentinel();
}
