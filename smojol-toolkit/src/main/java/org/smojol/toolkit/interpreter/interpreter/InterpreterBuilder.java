package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.vm.interpreter.*;

public interface InterpreterBuilder {
    CobolInterpreter wrap(CobolInterpreter interpreter);
}
