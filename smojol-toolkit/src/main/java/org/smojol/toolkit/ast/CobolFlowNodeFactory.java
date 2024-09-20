package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNodeImpl;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.NullFlowNode;
import org.smojol.common.ast.SyntaxIdentity;
import org.smojol.common.idms.IdmsContainerNode;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.vm.stack.StackFrames;

public class CobolFlowNodeFactory {
    public static FlowNode newNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.EvaluateStatementContext.class))
            return new EvaluateFlowNode(parseTree, scope, nodeService, stackFrames);
        if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.IfStatementContext.class))
            return new IfFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.GoToStatementContext.class))
            return new GoToFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.CallStatementContext.class))
            return new CallFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.ExitStatementContext.class))
            return new ExitFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.StopStatementContext.class))
            return new StopFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.NextSentenceContext.class) ||
                SyntaxIdentity.isOfType(parseTree, CobolParser.NextSentenceWrapperStatementContext.class))
            return new NextSentenceFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.satisfies(parseTree, SyntaxIdentity::PERFORM_PROCEDURE))
            return new PerformProcedureFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.satisfies(parseTree, SyntaxIdentity::PERFORM_INLINE))
            return new PerformInlineFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.SearchStatementContext.class))
            return new SearchFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.GenericOnClauseStatementContext.class))
            return new GenericOnClauseFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.DisplayStatementContext.class))
            return new DisplayFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.MoveStatementContext.class))
            return new MoveFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.ComputeStatementContext.class))
            return new ComputeFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.AddStatementContext.class))
            return new AddFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.SubtractStatementContext.class))
            return new SubtractFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.MultiplyStatementContext.class))
            return new MultiplyFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.DivideStatementContext.class))
            return new DivideFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isStatementOfType(parseTree, CobolParser.ComputeStatementContext.class))
            return new ComputeFlowNode(parseTree, scope, nodeService, stackFrames);

        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.DialectStatementContext.class))
            return DialectFlowNodeFactory.flowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ConditionalStatementCallContext.class))
            return new ConditionalStatementFlowNode(parseTree, scope, nodeService, stackFrames);
        // This needs to come last in all the statement classifications, or things will break
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.StatementContext.class))
            return new GenericStatementFlowNode(parseTree, scope, nodeService, stackFrames);

        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.PerformTypeContext.class))
            return new PerformTestFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, TerminalNodeImpl.class))
            return new SymbolFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ProcedureSectionHeaderContext.class))
            return new SectionHeaderFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ParagraphDefinitionNameContext.class))
            return new ParagraphNameFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.SentenceContext.class))
            return new SentenceFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.SearchWhenContext.class))
            return new SearchWhenFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.AtEndPhraseContext.class))
            return new AtEndPhraseFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.IfThenContext.class))
            return new IfThenFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.IfElseContext.class))
            return new IfElseFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ParagraphsContext.class))
            return new ParagraphsFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ParagraphContext.class))
            return new ParagraphFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ProcedureSectionContext.class))
            return new SectionFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ProcedureDivisionBodyContext.class))
            return new ProcedureDivisionBodyFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (isCompositeNode(parseTree))
            return new CompositeCobolFlowNode(parseTree, scope, nodeService, stackFrames);

        return new CobolFlowNode(parseTree, scope, nodeService, stackFrames);
    }

    public static FlowNode newUnmodifiedNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        if (SyntaxIdentity.isOfType(parseTree, CobolParser.StatementContext.class))
            return newUnmodifiedNode(parseTree.getChild(0), scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.EvaluateStatementContext.class))
            return new EvaluateFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.IfStatementContext.class))
            return new IfFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.GoToStatementContext.class))
            return new GoToFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.CallStatementContext.class))
            return new CallFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ExitStatementContext.class))
            return new ExitFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.StopStatementContext.class))
            return new StopFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.NextSentenceContext.class) ||
                SyntaxIdentity.isOfType(parseTree, CobolParser.NextSentenceWrapperStatementContext.class))
            return new NextSentenceFlowNode(parseTree, scope, nodeService, stackFrames);
//        else if (parseTree instanceof CobolParser.PerformStatementContext p && SyntaxIdentity.satisfies(parseTree, SyntaxIdentity::PERFORM_PROCEDURE))
        else if (parseTree instanceof CobolParser.PerformStatementContext p && p.performProcedureStatement() != null)
            return new PerformProcedureFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (parseTree instanceof CobolParser.PerformStatementContext p && p.performInlineStatement() != null)
            return new PerformInlineFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.SearchStatementContext.class))
            return new SearchFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.GenericOnClauseStatementContext.class))
            return new GenericOnClauseFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.DisplayStatementContext.class))
            return new DisplayFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.MoveStatementContext.class))
            return new MoveFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ComputeStatementContext.class))
            return new ComputeFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.AddStatementContext.class))
            return new AddFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.SubtractStatementContext.class))
            return new SubtractFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.MultiplyStatementContext.class))
            return new MultiplyFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.DivideStatementContext.class))
            return new DivideFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ComputeStatementContext.class))
            return new ComputeFlowNode(parseTree, scope, nodeService, stackFrames);

        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.DialectStatementContext.class))
            return DialectFlowNodeFactory.flowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ConditionalStatementCallContext.class))
            return new ConditionalStatementFlowNode(parseTree, scope, nodeService, stackFrames);
        // This needs to come last in all the statement classifications, or things will break

        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.PerformTypeContext.class))
            return new PerformTestFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, TerminalNodeImpl.class))
            return new SymbolFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ProcedureSectionHeaderContext.class))
            return new SectionHeaderFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ParagraphDefinitionNameContext.class))
            return new ParagraphNameFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.SentenceContext.class))
            return new SentenceFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.SearchWhenContext.class))
            return new SearchWhenFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.AtEndPhraseContext.class))
            return new AtEndPhraseFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.IfThenContext.class))
            return new IfThenFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.IfElseContext.class))
            return new IfElseFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ParagraphsContext.class))
            return new ParagraphsFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ParagraphContext.class))
            return new ParagraphFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ProcedureSectionContext.class))
            return new SectionFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (SyntaxIdentity.isOfType(parseTree, CobolParser.ProcedureDivisionBodyContext.class))
            return new ProcedureDivisionBodyFlowNode(parseTree, scope, nodeService, stackFrames);
        else if (isCompositeNode(parseTree))
            return new CompositeCobolFlowNode(parseTree, scope, nodeService, stackFrames);

        return new NullFlowNode();
    }

    private static boolean isCompositeNode(ParseTree executionContext) {
        return
                executionContext.getClass() == CobolParser.DialectSectionContext.class ||
                executionContext.getClass() == IdmsParser.IdmsIfStatementContext.class ||
                executionContext.getClass() == IdmsContainerNode.class ||
                executionContext.getClass() == IdmsParser.InquireMapIfStatementContext.class
                ;
    }
}
