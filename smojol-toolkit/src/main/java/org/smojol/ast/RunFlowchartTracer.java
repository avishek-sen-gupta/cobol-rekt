package org.smojol.ast;

import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.model.MutableNode;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.ChartOverlay;
import org.smojol.common.flowchart.GraphWriter;
import org.smojol.common.vm.interpreter.ExecutionListener;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class RunFlowchartTracer implements ExecutionListener, GraphWriter {
    private Set<FlowNode> nodes = new HashSet<>();

    @Override
    public void notify(String message, FlowNode node, FlowNodeService nodeService) {
    }

    @Override
    public void visit(FlowNode node, FlowNodeService nodeService) {
        nodes.add(node);
    }

    @Override
    public void notifyTermination() {
    }

    @Override
    public void process(MutableGraph g, ChartOverlay overlay) {
        System.out.println("Collected " + nodes.size() + " nodes");
        final int[] i = {1};
        nodes.stream().filter(n -> !n.isPassthrough()).forEach(n -> {
            System.out.println(i[0]);
            i[0]++;
//            System.out.println("Processing node: " + n.getClass().getSimpleName() + "/" + n.id());
            FlowNode block = overlay.block(n);
//            System.out.println("Processing overlay: " + block.getClass().getSimpleName() + "/" + block.id());
            List<MutableNode> matchingNodes = g.nodes().stream().filter(mn -> mn.name().toString().equals(block.id())).toList();
            matchingNodes.forEach(mn -> mn.add("style", "bold, filled").add("fillcolor", Color.GREENYELLOW.value).add("fontcolor", Color.BLACK.value));
        });
    }
}
