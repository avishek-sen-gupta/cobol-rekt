package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class DialectStatementFlowNode extends CobolFlowNode {
    private FlowNode idmsChildNode;
    private boolean databaseAccess = false;

    public DialectStatementFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        if (idmsChildNode.getClass() == CobolFlowNode.class) return;
        visitor.visitParentChildLink(this, idmsChildNode, new VisitContext(level), nodeService);
        idmsChildNode.accept(visitor, -1);
    }

    // TODO: Rewrite this monstrosity
    @Override
    public String name() {
        return truncated(originalText(), 30);
    }

    @Override
    public void buildInternalFlow() {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        ParseTree containerChild = executionContext.getChild(0);
        System.out.println("IDMS DATA: " + containerChild.getText());
        // TODO: Replace with proper type checking
        ParseTree possibleDbAccessStatement = navigator.findByCondition(containerChild, n -> n.getClass() == IdmsParser.ObtainStatementContext.class ||
                n.getClass() == IdmsParser.PutStatementContext.class ||
                n.getClass() == IdmsParser.FindStatementContext.class ||
                n.getClass() == IdmsParser.GetStatementContext.class);

        if (possibleDbAccessStatement != null) {
            System.out.println("FOUND DB ACCESS");
            databaseAccess = true;
        }

        if (navigator.findByCondition(containerChild, n -> n.getClass() == IdmsParser.TransferStatementContext.class) != null) {
            idmsChildNode = new IdmsTransferFlowNode(containerChild, this, nodeService, staticFrameContext);
            nodeService.register(idmsChildNode);
            System.out.println("Found a TRANSFER statement");
        } else if (containerChild.getClass() == CobolParser.DialectIfStatmentContext.class) {
            idmsChildNode = new IdmsIfFlowNode(containerChild, this, nodeService, staticFrameContext);
            nodeService.register(idmsChildNode);
        } else {
            // Treat everything as an IDMS statement for now
            idmsChildNode = nodeService.node(
                    navigator.findByCondition(executionContext,
                            n -> n.getClass() == IdmsParser.IdmsStatementsContext.class), this, staticFrameContext);
//            idmsChildNode = idmsContainerChartNode(executionContext);
        }
        idmsChildNode.buildFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.DIALECT;
    }

    @Override
    public boolean accessesDatabase() {
        return databaseAccess;
    }

    @Override
    public String originalText() {
        return NodeText.idmsOriginalText(getExecutionContext(), nodeService);
    }

    @Override
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.DIALECT);
    }
}
