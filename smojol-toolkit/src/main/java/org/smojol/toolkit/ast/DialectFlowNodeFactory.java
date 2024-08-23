package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;

public class DialectFlowNodeFactory {
    public static FlowNode flowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        CobolParser.DialectStatementContext dialectStatement = (CobolParser.DialectStatementContext) parseTree;
        CobolEntityNavigator navigator = nodeService.getNavigator();
        IdmsParser.BindRunUnitClauseContext bindRunUnitIndicator = navigator.findByCondition(dialectStatement, IdmsParser.BindRunUnitClauseContext.class);
        if (bindRunUnitIndicator != null) {
            IdmsParser.BindStatementContext bindStatement = (IdmsParser.BindStatementContext) bindRunUnitIndicator.getParent();
            return new BindStatementFlowNode(bindStatement, scope, nodeService, stackFrames);
        }

        IdmsParser.FinishStatementContext finishStatement = navigator.findByCondition(dialectStatement, IdmsParser.FinishStatementContext.class);
        if (finishStatement != null) {
            return new FinishStatementFlowNode(finishStatement, scope, nodeService, stackFrames);
        }

        return new DialectStatementFlowNode(parseTree, scope, nodeService, stackFrames);
    }
}
