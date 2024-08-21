package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class SearchFlowNode extends CobolFlowNode {
    private FlowNode atEndBlock;
    private List<FlowNode> whenPhrases;
    @Getter private CobolParser.QualifiedDataNameContext searchTerm;

    public SearchFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.SearchStatementContext searchStatementContext = new SyntaxIdentity<CobolParser.SearchStatementContext>(executionContext).get();
        searchTerm = searchStatementContext.qualifiedDataName();
        if (endPhraseExists()) {
            atEndBlock = nodeService.node(searchStatementContext.atEndPhrase(), this, staticFrameContext);
            atEndBlock.buildFlow();
        }
        List<CobolParser.SearchWhenContext> searchWhenContexts = searchStatementContext.searchWhen();
        whenPhrases = searchWhenContexts.stream().map(when -> nodeService.node(when, this, staticFrameContext)).toList();

        whenPhrases.forEach(FlowNode::buildFlow);
    }

    private boolean endPhraseExists() {
        CobolParser.SearchStatementContext searchStatementContext = new SyntaxIdentity<CobolParser.SearchStatementContext>(executionContext).get();
        return searchStatementContext.atEndPhrase() != null;
    }

    @Override
    public void buildOutgoingFlow() {
        // Call super here because this is still a normal statement which will continue its normal flow, after PERFORM returns
        super.buildOutgoingFlow();
    }

    @Override
    public void buildControlFlow() {
        if (endPhraseExists()) atEndBlock.buildControlFlow();
        whenPhrases.forEach(FlowNode::buildControlFlow);
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        whenPhrases.forEach(w -> w.acceptUnvisited(visitor, level));
        whenPhrases.forEach(w -> visitor.visitParentChildLink(this, w, new VisitContext(level), nodeService));

        if (endPhraseExists()) {
            atEndBlock.accept(visitor, level);
            visitor.visitParentChildLink(this, atEndBlock, new VisitContext(level), nodeService);
        }
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.SEARCH;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        return interpreter.scope(this).executeSearch(atEndBlock, whenPhrases, nodeService, this);
    }

    @Override
    public String label() {
        CobolParser.SearchStatementContext searchStatementContext = new SyntaxIdentity<CobolParser.SearchStatementContext>(executionContext).get();
        return searchStatementContext.SEARCH().getText() + "\n" + searchStatementContext.qualifiedDataName().getText();
//        return CobolContextAugmentedTreeNode.originalText(executionContext, CobolEntityNavigator::PASSTHROUGH);
    }

    @Override
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.SEARCH);
    }
}