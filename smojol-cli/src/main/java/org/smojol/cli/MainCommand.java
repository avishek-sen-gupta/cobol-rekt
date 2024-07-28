package org.smojol.cli;

import picocli.CommandLine.Command;

import java.util.concurrent.Callable;

@Command(name = "subcommands", subcommands = {FlowchartCommand.class, GraphDBCommand.class})
public class MainCommand implements Callable<Integer> {

    @Override
    public Integer call() {
        System.out.println("Subcommand needed: one of flowchart");
        return 0;
    }
}
