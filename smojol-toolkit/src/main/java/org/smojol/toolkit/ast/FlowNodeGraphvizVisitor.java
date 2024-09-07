package org.smojol.toolkit.ast;

import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.model.MutableNode;
import org.smojol.common.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.toolkit.flowchart.FlowchartStylePreferences;

import java.util.List;
import java.util.UUID;
import java.util.function.Function;
import java.util.logging.Logger;

import static guru.nidi.graphviz.model.Factory.*;

public class FlowNodeGraphvizVisitor implements FlowNodeVisitor {
    private static final Logger logger = Logger.getLogger(FlowNodeGraphvizVisitor.class.getName());
    int maxLevel = -1;
    private final Function<VisitContext, Boolean> visitCondition;
    private final MutableGraph g;
    private final ChartOverlay overlay;

    public FlowNodeGraphvizVisitor(MutableGraph g, ChartOverlay overlay, Function<VisitContext, Boolean> stopCondition) {
        this.g = g;
        this.overlay = overlay;
        this.visitCondition = stopCondition;
    }

    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext visitContext, FlowNodeService nodeService) {
        if (!visitCondition.apply(visitContext)) return;
        if (node.isPassthrough()) return;
        logger.finer("Visiting : " + node);
        FlowNode source = overlay.block(node);
        List<FlowNode> targets = outgoingNodes.stream().map(FlowNode::passthrough).map(overlay::block).toList();

        MutableNode flowchartSource = existingOrNew(source.id());
        targets.forEach(t -> {
            logger.finer("Linking " + node + " to " + t);
            if (source == t) return;
            MutableNode graphSource = styled(source, flowchartSource.add("label", source.label()));
            MutableNode flowchartTarget = existingOrNew(t.id());
            MutableNode graphTarget = styled(t, flowchartTarget.add("label", t.label()));
            g.add(graphSource.addLink(graphSource.linkTo(graphTarget).with("penwidth", "3")));
        });

        if (node.accessesDatabase()) {
            g.add(flowchartSource.addLink(existingOrNew("IDMS Database")
                    .add("shape", "cylinder")
                    .add("height", "4")
                    .add("width", "4")
                    .add("style", "filled")
                    .add("fillcolor", Color.LIGHTBLUE2.value)
                    .add("penwidth", "4")
            ));
        }
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService) {
        if (!visitCondition.apply(visitContext.oneLower())) return;
        visitParentChildLink(parent, internalTreeRoot, visitContext, nodeService, FlowNodeCondition.ALWAYS_SHOW);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        if (!visitCondition.apply(visitContext)) return;
        FlowNode overlayParent = overlay.block(parent);
        if (overlayParent.getClass() == GenericProcessingFlowNode.class) return;
        FlowNode passthroughTarget = internalTreeRoot.passthrough();
        FlowNode overlayInternalTreeRoot = overlay.block(passthroughTarget);
        MutableNode graphParent = styled(overlayParent, existingOrNew(overlayParent.id())).add("label", overlayParent.label());
        MutableNode graphChild = existingOrNew(overlayInternalTreeRoot.id()).add("label", overlayInternalTreeRoot.label());
        MutableNode child = styled(overlayInternalTreeRoot, graphChild);
//        if (overlayInternalTreeRoot.getExecutionContext() != null && overlayInternalTreeRoot.getExecutionContext().getClass() == CobolParser.ConditionalStatementCallContext.class)
        String arrowStyle = hideStrategy.apply(overlayInternalTreeRoot) ? "none" : "normal";
        graphParent.addLink(graphParent.linkTo(child).with("style", "dashed").with("arrowhead", arrowStyle));
    }

    private MutableNode existingOrNew(String nodeID) {
        List<MutableNode> matchingNodes = g.nodes().stream().filter(gn -> gn.name().toString().equals(nodeID)).toList();
        if (matchingNodes.isEmpty()) {
            MutableNode newNode = mutNode(nodeID);
            g.add(newNode);
            return newNode;
        }
        if (matchingNodes.size() > 1) {
            throw new RuntimeException(String.format("Duplicate node detected, %s copies found of ID: %s", matchingNodes.size(), matchingNodes.getFirst().name()));
        }
        return matchingNodes.getFirst();
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        if (!visitCondition.apply(visitContext)) return;
        FlowNode overlayFrom = overlay.block(from.passthrough());
        FlowNode overlayTo = overlay.block(to.passthrough());
        MutableNode origin = styled(overlayFrom, existingOrNew(overlayFrom.id())).add("label", overlayFrom.label());
        MutableNode destination = styled(overlayTo, existingOrNew(overlayTo.id())).add("label", overlayTo.label());
        origin.addLink(origin.linkTo(destination).with("style", "bold").with("color", "blueviolet"));
    }


    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {
        FlowNodeCollectorVisitor collector = new FlowNodeCollectorVisitor(overlay);
        root.accept(collector, -1);
        List<FlowNode> flowNodes = collector.getFlowNodes();
        String clusterID = String.format("cluster_%s", UUID.randomUUID());
        MutableGraph outliningCluster = mutGraph(clusterID).setCluster(true).graphAttrs().add("bgcolor", Color.LIGHTGREY.value);
        flowNodes.forEach(n -> {
            outliningCluster.add(existingOrNew(n.id()));
        });
        g.add(outliningCluster);
    }

    private MutableNode styled(FlowNode flowNode, MutableNode node) {
        return FlowchartStylePreferences.scheme(flowNode).apply(node);
    }
}
