package org.smojol.toolkit.intermediate;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.NullFlowNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.pseudocode.IncrementingIdProvider;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.CompositeCobolFlowNode;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import org.smojol.toolkit.interpreter.stack.CobolStackFrames;

import java.util.List;
import java.util.stream.IntStream;

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
        FlowNode root = recursivelyBuildIntermediateNode(codeRoot, null, stackFrames);
        root.buildTwin();
        root.buildControlFlow();
        root.resolve(symbolTable, dataRoot);
        return root;
    }

    private FlowNode recursivelyBuildIntermediateNode(ParseTree current, FlowNode parent, StackFrames stackFrames) {
        FlowNode intermediateNode = nodeService.unmodifiedNode(current, parent, stackFrames);
        if (intermediateNode instanceof NullFlowNode) return intermediateNode;
        if (!(intermediateNode instanceof CompositeCobolFlowNode)) return intermediateNode;
        List<ParseTree> children = IntStream.range(0, current.getChildCount()).mapToObj(current::getChild).toList();
        List<FlowNode> flowChildren = children.stream().map(child -> recursivelyBuildIntermediateNode(child, intermediateNode, stackFrames.add(intermediateNode))).toList();
        List<FlowNode> validFlowChildren = flowChildren.stream().filter(n -> !(n instanceof NullFlowNode)).toList();
        validFlowChildren.forEach(intermediateNode::addChild);
        return intermediateNode;
    }
}
