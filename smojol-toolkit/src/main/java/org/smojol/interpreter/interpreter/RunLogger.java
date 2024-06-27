package org.smojol.interpreter.interpreter;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.ast.AddFlowNode;
import org.smojol.ast.IfFlowNode;
import org.smojol.ast.MoveFlowNode;
import org.smojol.common.vm.interpreter.ExecutionListener;

import java.io.FileWriter;
import java.io.IOException;

public class RunLogger implements ExecutionListener {

    private final StringBuilder sb = new StringBuilder();
    private String path = "/Users/asgupta/Downloads/mbrdi-poc/report.md";

    public void close() throws IOException {
        write();
    }

    private void write() throws IOException {
        FileWriter writer = new FileWriter(path, false);
        writer.flush();
        writer.write(sb.toString());
        writer.flush();
        writer.close();
    }

    @Override
    public void visit(String message, FlowNode node, FlowNodeService nodeService) {
        if (node.getClass() == MoveFlowNode.class ||
                node.getClass() == AddFlowNode.class ||
                node.getClass() == IfFlowNode.class)
            sb.append(message + "\n");
        System.out.println(message);
    }

    @Override
    public void notify(String message, FlowNode node, FlowNodeService nodeService) {
        visit(message, node, nodeService);
    }

    @Override
    public void notifyTermination() {
        try {
            write();
        } catch (IOException e) {
            System.err.println("WARNING: Couldn't write to file: " + e.getMessage());
        }
    }
}
