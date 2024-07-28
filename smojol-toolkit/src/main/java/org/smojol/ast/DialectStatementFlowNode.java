package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;

public class DialectStatementFlowNode extends CobolFlowNode {
    private FlowNode idmsChildNode;
    private boolean databaseAccess = false;

    public DialectStatementFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
//        visitor.visitParentChildLink(this, idmsChildNode, nodeService);
//        idmsChildNode.accept(visitor, level, maxLevel);
    }

    // TODO: Rewrite this monstrosity
    @Override
    public String name() {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        ParseTree dialectGuidContext = navigator.findByCondition(executionContext, t -> t.getClass() == CobolParser.DialectGuidContext.class);
        String guid = dialectGuidContext.getText();

        ParseTree idmsTextNode = PersistentData.getDialectNode("IDMS-" + guid);
        String codeText = NodeText.originalText(idmsTextNode, NodeText::PASSTHROUGH);
        return truncated(codeText, 30);
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

        if (containerChild.getClass() == CobolParser.DialectIfStatmentContext.class) {
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
}
