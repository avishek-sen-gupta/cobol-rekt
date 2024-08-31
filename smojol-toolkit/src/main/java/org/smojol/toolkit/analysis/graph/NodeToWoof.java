package org.smojol.toolkit.analysis.graph;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.NodeSpec;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNodeLike;
import org.smojol.common.program.CobolProgram;
import org.smojol.common.ast.CommentBlock;
import org.smojol.common.vm.structure.CobolDataStructure;

public class NodeToWoof {
    public static Record newOrExistingCFGNode(FlowNodeLike node, GraphSDK sdk, NodeSpecBuilder qualifier) {
        return sdk.newOrExisting(qualifier.cfgNodeSearchSpec(node), toWoofNode(node, qualifier));
    }

    public static Record existingCFGNode(FlowNodeLike node, NodeSpecBuilder qualifier, GraphSDK sdk) {
        return sdk.existing(qualifier.cfgNodeSearchSpec(node));
    }

    public static WoofNode toWoofNode(FlowNodeLike node, NodeSpecBuilder qualifier) {
        return new WoofNode(qualifier.newCFGNode(node));
    }

    public static WoofNode toWoofTraceNode(FlowNodeLike node, NodeSpecBuilder qualifier) {
        return new WoofNode(qualifier.newTraceNode(node));
    }

    public static WoofNode dataStructureToWoof(CobolDataStructure data, NodeSpecBuilder specBuilder) {
        NodeSpec spec = specBuilder.newDataNode(data);
        return new WoofNode(spec.properties(), spec.labels());
    }

    public static WoofNode toWoofNode(CommentBlock node, NodeSpecBuilder qualifier) {
        return new WoofNode(qualifier.commentNode(node));
    }

    public static WoofNode programToWoof(CobolProgram cobolProgram, NodeSpecBuilder qualifier) {
        return new WoofNode(qualifier.program(cobolProgram));
    }
}
