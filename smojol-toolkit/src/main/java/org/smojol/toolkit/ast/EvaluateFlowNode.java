package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.EvaluateBreaker;
import org.smojol.common.vm.expression.ExpandedEvaluation;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

@Getter
public class EvaluateFlowNode extends CobolFlowNode {
    private final List<CobolParser.EvaluateSelectContext> evaluationChannels = new ArrayList<>();
    private List<EvaluateBranchFlowNode> whenPhrases;
    private List<CobolExpression> evaluationSubjects = new ArrayList<>();
    private List<Pair<CobolExpression, List<FlowNode>>> whenPhraseFlowNodes;
    private ExpandedEvaluation deconstructedRepresentation;

    public EvaluateFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.EvaluateStatementContext whenStatement = new SyntaxIdentity<CobolParser.EvaluateStatementContext>(executionContext).get();
        evaluationChannels.add(whenStatement.evaluateSelect());
        evaluationChannels.addAll(whenStatement.evaluateAlsoSelect().stream().map(CobolParser.EvaluateAlsoSelectContext::evaluateSelect).toList());
        whenPhrases = whenStatement.evaluateWhenPhrase().stream().map(ewp -> new EvaluateBranchFlowNode(ewp, this, nodeService, staticFrameContext)).toList();

//        CobolParser.EvaluateStatementContext whenStatement = new SyntaxIdentity<CobolParser.EvaluateStatementContext>(getExecutionContext()).get();
        deconstructedRepresentation = new EvaluateBreaker(staticFrameContext, this, nodeService).decompose(whenStatement);
        deconstructedRepresentation.buildFlow();
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.EVALUATE;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeIf(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public String name() {
        CobolParser.EvaluateStatementContext evaluateStatement = new SyntaxIdentity<CobolParser.EvaluateStatementContext>(getExecutionContext()).get();
        return "EVALUATE";
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.DECISION);
    }

    @Override
    public List<FlowNode> astChildren() {
        return ImmutableList.of();
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        deconstructedRepresentation.resolve(symbolTable, dataStructures);
    }
}
