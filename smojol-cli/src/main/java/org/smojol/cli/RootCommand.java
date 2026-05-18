package org.smojol.cli;

import java.util.concurrent.Callable;
import picocli.CommandLine.Command;

@Command(
    name = "app",
    mixinStandardHelpOptions = true,
    version = "sourceGraph 0.1",
    subcommands = {
      MultiCommand.class,
      DependencyAnalysisCommand.class,
      ValidateCommand.class,
      InterpretCommand.class
    },
    description = "Implements various operations useful for reverse engineering Cobol code")
public class RootCommand implements Callable<Integer> {
  @Override
  public Integer call() {
    return 0;
  }
}
