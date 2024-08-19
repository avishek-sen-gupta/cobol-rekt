package org.smojol.common.program;

public interface ProgramVisitor {
    ProgramVisitor visit(CobolProgram cobolProgram);
}
