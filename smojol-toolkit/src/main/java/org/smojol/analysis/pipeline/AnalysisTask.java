package org.smojol.analysis.pipeline;

public enum AnalysisTask {
    INJECT_INTO_NEO4J,
    EXPORT_TO_GRAPHML,
    WRITE_RAW_AST,
    WRITE_FLOW_AST,
    DRAW_FLOWCHART,
    WRITE_CFG,
    ATTACH_COMMENTS,
    WRITE_DATA_STRUCTURES
}
