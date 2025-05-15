package com.mojo.algorithms.id;


import com.mojo.algorithms.CodeSentinelType;

public interface InstructionLike extends Identifiable {
    CodeSentinelType sentinel();
}
