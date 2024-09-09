package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.interpreter.ExecutionListener;

import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Logger;

public class RunLogger implements ExecutionListener {
    private static final Logger LOGGER = Logger.getLogger(RunLogger.class.getName());
    private final StringBuilder sb = new StringBuilder();
    private final String path;

    public RunLogger(String outputPath) {
        this.path = outputPath;
    }

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
    public void notify(String message, FlowNode node, FlowNodeService nodeService) {
//        if (node.getClass() == MoveFlowNode.class ||
//                node.getClass() == AddFlowNode.class ||
//                node.getClass() == IfFlowNode.class)
//            sb.append(message).append("\n");
//        System.out.println(message);
        LOGGER.info(message);
    }

    @Override
    public void visit(FlowNode node, FlowNodeService nodeService) {
    }

    @Override
    public void visitTermination() {
        try {
            write();
        } catch (IOException e) {
            System.err.println("WARNING: Couldn't write to file: " + e.getMessage());
        }
    }

    @Override
    public void notifyTermination() {

    }
}
