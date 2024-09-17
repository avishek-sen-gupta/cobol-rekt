package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.idms.IdmsContainerNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;
import java.util.logging.Logger;

public class DialectStatementFlowNode extends CobolFlowNode {
    private static final Logger LOGGER = Logger.getLogger(DialectStatementFlowNode.class.getName());
    private FlowNode dialectChildNode;
    private boolean databaseAccess = false;

    public DialectStatementFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        if (dialectChildNode.getClass() == CobolFlowNode.class) return;
        visitor.visitParentChildLink(this, dialectChildNode, new VisitContext(level), nodeService);
        dialectChildNode.accept(visitor, -1);
    }

    // TODO: Rewrite this monstrosity
    @Override
    public String name() {
        return truncated(originalText(), 30);
    }

    @Override
    public void buildInternalFlow() {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        IdmsContainerNode containerNode = (IdmsContainerNode) navigator.findByCondition(n -> n.getClass() == IdmsContainerNode.class);
        LocalisedDialect dialect = containerNode.getDialect();
        switch (dialect) {
            case IDMS: buildIdmsFlow(navigator);
                       break;
            case CICS: buildCicsFLow(containerNode);
                       break;
        }
    }

    private void buildCicsFLow(IdmsContainerNode containerNode) {
        dialectChildNode = new CicsBlockFlowNode(containerNode, this, nodeService, staticFrameContext);
    }

    private void buildIdmsFlow(CobolEntityNavigator navigator) {
        ParseTree containerChild = executionContext.getChild(0);
        LOGGER.finer("IDMS DATA: " + containerChild.getText());
        // TODO: Replace with proper type checking
        ParseTree possibleDbAccessStatement = navigator.findByCondition(containerChild, n -> n.getClass() == IdmsParser.ObtainStatementContext.class ||
                n.getClass() == IdmsParser.PutStatementContext.class ||
                n.getClass() == IdmsParser.FindStatementContext.class ||
                n.getClass() == IdmsParser.GetStatementContext.class);

        if (possibleDbAccessStatement != null) {
            LOGGER.finer("FOUND DB ACCESS");
            databaseAccess = true;
        }

        if (navigator.findByCondition(containerChild, n -> n.getClass() == IdmsParser.TransferStatementContext.class) != null) {
            dialectChildNode = new IdmsTransferFlowNode(containerChild, this, nodeService, staticFrameContext);
            nodeService.register(dialectChildNode);
            LOGGER.finer("Found a TRANSFER statement");
        } else if (containerChild.getClass() == CobolParser.DialectIfStatmentContext.class) {
            dialectChildNode = new IdmsIfFlowNode(containerChild, this, nodeService, staticFrameContext);
            nodeService.register(dialectChildNode);
        } else {
            // Treat everything as an IDMS statement for now
            dialectChildNode = nodeService.node(
                    navigator.findByCondition(executionContext,
                            n -> n.getClass() == IdmsParser.IdmsStatementsContext.class), this, staticFrameContext);
//            idmsChildNode = idmsContainerChartNode(executionContext);
        }
        dialectChildNode.buildFlow();
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
        return NodeText.dialectOriginalText(getExecutionContext(), nodeService);
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.DIALECT);
    }

    @Override
    public List<FlowNode> astChildren() {
        return ImmutableList.of(dialectChildNode);
    }
}
