package org.smojol.cli;

import picocli.CommandLine.Command;

import java.util.concurrent.Callable;

@Command(name = "app", mixinStandardHelpOptions = true, version = "graph 0.1",
        subcommands = {MultiCommand.class, DependencyAnalysisCommand.class, ValidateCommand.class, InterpretCommand.class},
        description = "Implements various operations useful for reverse engineering Cobol code")
public class RootCommand implements Callable<Integer> {
    @Override
    public Integer call() {
        return 0;
    }
}
