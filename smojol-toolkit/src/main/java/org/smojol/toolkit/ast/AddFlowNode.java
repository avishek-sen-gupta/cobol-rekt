package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.CobolExpressionBuilder;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

@Getter
public class AddFlowNode extends CobolFlowNode {
    private List<CobolParser.AddFromContext> froms;
    private List<CobolParser.AddToContext> tos;
    private List<CobolParser.AddGivingContext> givingDestinations;
    private List<CobolParser.AddToGivingContext> tosGiving;
    private List<CobolExpression> toExpressions;
    private List<CobolExpression> fromExpressions;
    private List<CobolExpression> tosGivingExpressions;
    private List<CobolExpression> givingDestinationExpressions;
    private final List<CobolExpression> destinationExpressions = new ArrayList<>();
    private final List<CobolExpression> sourceExpressions = new ArrayList<>();

    public AddFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.AddStatementContext addStatement = new SyntaxIdentity<CobolParser.AddStatementContext>(executionContext).get();

        if (addStatement.addToStatement() != null) {
            froms = addStatement.addToStatement().addFrom();
            tos = addStatement.addToStatement().addTo();
            tosGiving = ImmutableList.of();
            givingDestinations = ImmutableList.of();
        }
        else if (addStatement.addToGivingStatement() != null) {
            froms = addStatement.addToGivingStatement().addFrom();
            tos = ImmutableList.of();
            tosGiving = addStatement.addToGivingStatement().addToGiving();
            givingDestinations = addStatement.addToGivingStatement().addGiving();
        }
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.ADD;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.COMPUTATIONAL);
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeAdd(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public boolean isMergeable() {
        return true;
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        toExpressions = tos.stream().map(to -> builder.identifier(to.generalIdentifier())).toList();
        fromExpressions = froms.stream().map(from -> builder.literalOrIdentifier(from.literal(), from.generalIdentifier())).toList();
        tosGivingExpressions = tosGiving.stream().map(from -> builder.literalOrIdentifier(from.literal(), from.generalIdentifier())).toList();
        givingDestinationExpressions = givingDestinations.stream().map(dest -> builder.identifier(dest.generalIdentifier())).toList();

        if (givingDestinationExpressions.isEmpty()) {
            sourceExpressions.addAll(fromExpressions);
            sourceExpressions.addAll(toExpressions);
            destinationExpressions.addAll(toExpressions);
        } else {
            sourceExpressions.addAll(fromExpressions);
            sourceExpressions.addAll(tosGivingExpressions);
            destinationExpressions.addAll(givingDestinationExpressions);
        }
    }
}
