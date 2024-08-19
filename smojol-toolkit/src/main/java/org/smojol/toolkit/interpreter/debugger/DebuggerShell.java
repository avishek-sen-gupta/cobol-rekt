package org.smojol.toolkit.interpreter.debugger;

import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.smojol.common.vm.stack.ExecutionContext;

import java.io.IOException;

import static org.smojol.common.flowchart.ConsoleColors.cyan;
import static org.smojol.common.flowchart.ConsoleColors.red;

public class DebuggerShell {
    public boolean interpret(String line, ExecutionContext executionContext) {
        if (line.contains("stack")) {
            System.out.println(cyan("STACK TRACE\n-------------------------\n"));
            System.out.println(cyan(executionContext.runtimeStackFrames().stackTrace()));
            return true;
        } else if ("c".equals(line.trim())) return false;
        else if ("q".equals(line.trim())) {
            System.out.println(red("Terminating program early..."));
            System.exit(0);
        } else if (line.trim().isEmpty()) return true;
        else {
            System.out.printf("%s=%s%n", line.trim(), executionContext.runtimeStackFrames().currentData().value(line.trim()));
        }
        return true;
    }

    public void run(ExecutionContext executionContext) {
        String prompt = ">> ";
        try {
            Terminal terminal = TerminalBuilder.builder().dumb(true).build();
            LineReader reader = LineReaderBuilder.builder().terminal(terminal).build();
            while(interpret(reader.readLine(prompt), executionContext)) {
            }
        } catch (IOException e) {
            System.out.printf(e.getMessage());
        }
    }
}
