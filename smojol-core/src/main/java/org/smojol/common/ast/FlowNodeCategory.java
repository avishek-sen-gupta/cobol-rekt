package org.smojol.common.ast;

public enum FlowNodeCategory {
    COMPUTATIONAL,
    DATA_FLOW,
    SEARCH,
    CONTROL_FLOW,
    TERMINAL,
    CODE_BLOCK,
    DECISION,
    CODE_LABEL,
    GENERIC_CODE,
    UNKNOWN,
    IO,
    DIALECT,
    PROGRAM,
    METADATA,
    PLACEHOLDER, LOOP, SYMBOL, CODE_ROOT
}
