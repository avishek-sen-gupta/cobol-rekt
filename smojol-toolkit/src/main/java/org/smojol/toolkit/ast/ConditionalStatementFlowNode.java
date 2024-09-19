package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class ConditionalStatementFlowNode extends CobolFlowNode {
    @Getter private FlowNode actualStatement;

    public ConditionalStatementFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildControlFlow() {
        actualStatement.buildControlFlow();
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.ConditionalStatementCallContext stmt = (CobolParser.ConditionalStatementCallContext) executionContext;
        if (stmt.statement() != null)
            actualStatement = nodeService.node(stmt.statement(), this, staticFrameContext);
        else
            actualStatement = nodeService.node(stmt.dialectStatement(), this, staticFrameContext);
        actualStatement.buildFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.CONDITIONAL_STATEMENT;
    }

    @Override
    public String name() {
        return executionContext.getText();
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.CODE_BLOCK);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        actualStatement.resolve(symbolTable, dataStructures);
    }

//    @Override
//    public List<FlowNode> astChildren() {
//        return ImmutableList.of(actualStatement);
//    }
}
