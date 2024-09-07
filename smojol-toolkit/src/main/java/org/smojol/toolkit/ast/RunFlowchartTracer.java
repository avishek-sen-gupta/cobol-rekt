package org.smojol.toolkit.ast;

import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.model.MutableNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.flowchart.ChartOverlay;
import org.smojol.common.flowchart.GraphWriter;
import org.smojol.common.vm.interpreter.ExecutionListener;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

public class RunFlowchartTracer implements ExecutionListener, GraphWriter {
    private static final Logger LOGGER = Logger.getLogger(RunFlowchartTracer.class.getName());
    private Set<FlowNode> nodes = new HashSet<>();

    @Override
    public void notify(String message, FlowNode node, FlowNodeService nodeService) {
    }

    @Override
    public void visit(FlowNode node, FlowNodeService nodeService) {
        nodes.add(node);
    }

    @Override
    public void visitTermination() {
    }

    @Override
    public void notifyTermination() {

    }

    @Override
    public void process(MutableGraph g, ChartOverlay overlay) {
        LOGGER.info("Collected " + nodes.size() + " nodes");
        final int[] i = {1};
        nodes.stream().filter(n -> !n.isPassthrough()).forEach(n -> {
            LOGGER.finer(String.valueOf(i[0]));
            i[0]++;
//            System.out.println("Processing node: " + n.getClass().getSimpleName() + "/" + n.id());
            FlowNode block = overlay.block(n);
//            System.out.println("Processing overlay: " + block.getClass().getSimpleName() + "/" + block.id());
            List<MutableNode> matchingNodes = g.nodes().stream().filter(mn -> mn.name().toString().equals(block.id())).toList();
            matchingNodes.forEach(mn -> mn.add("style", "bold, filled").add("fillcolor", Color.GREENYELLOW.value).add("fontcolor", Color.BLACK.value));
        });
    }
}
