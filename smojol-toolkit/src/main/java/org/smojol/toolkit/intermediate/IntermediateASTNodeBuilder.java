package org.smojol.toolkit.intermediate;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTTraversal;
import org.smojol.common.ast.NullFlowNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.pseudocode.IncrementingIdProvider;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.transpiler.TranspilerSetup;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.CompositeCobolFlowNode;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import org.smojol.toolkit.interpreter.stack.CobolStackFrames;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class IntermediateASTNodeBuilder {
    private final CobolDataStructure dataRoot;
    private final ParserRuleContext codeRoot;
    private final SmojolSymbolTable symbolTable;
    private final FlowNodeServiceImpl nodeService;

    public IntermediateASTNodeBuilder(ParserRuleContext codeRoot, CobolDataStructure dataRoot, SmojolSymbolTable symbolTable) {
        this.dataRoot = dataRoot;
        this.codeRoot = codeRoot;
        this.symbolTable = symbolTable;
        nodeService = new FlowNodeServiceImpl(new CobolEntityNavigator(codeRoot), dataRoot, new IncrementingIdProvider());
    }

    public FlowNode build() {
        StackFrames stackFrames = new CobolStackFrames();
        FlowNode root = recursivelyVisit(codeRoot, null, stackFrames);
        root.buildTwin();
        root.buildControlFlow();
        TranspilerSetup.buildSymbolTable(root, dataRoot, symbolTable);
        return root;
    }

    private FlowNode recursivelyVisit(ParseTree current, FlowNode parent, StackFrames stackFrames) {
        FlowNode intermediateNode = nodeService.unmodifiedNode(current, parent, stackFrames);
        if (intermediateNode instanceof NullFlowNode) return intermediateNode;
        if (!(intermediateNode instanceof CompositeCobolFlowNode)) return intermediateNode;
        List<ParseTree> children = new ArrayList<>();
        for (int i = 0; i < current.getChildCount(); i++) {
            children.add(current.getChild(i));
        }
        List<FlowNode> flowNodeStream = children.stream().map(child -> recursivelyVisit(child, intermediateNode, stackFrames.add(intermediateNode))).toList();
        List<FlowNode> flowChildren = flowNodeStream.stream().filter(n -> !(n instanceof NullFlowNode)).toList();
        flowChildren.forEach(intermediateNode::addChild);
        return intermediateNode;
    }
}
