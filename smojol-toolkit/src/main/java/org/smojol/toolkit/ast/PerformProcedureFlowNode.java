package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.FlowIterationBuilder;
import org.smojol.common.vm.expression.FlowIteration;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class PerformProcedureFlowNode extends CobolFlowNode implements InternalControlFlowNode {
    private static final Logger logger = Logger.getLogger(PerformProcedureFlowNode.class.getName());
    private FlowNode inlineStatementContext;
    private final List<FlowNode> procedures = new ArrayList<>();
    private FlowNode condition;
    @Getter
    private List<FlowIteration> nestedLoops;
    @Getter
    private FlowNode startNode;
    @Getter
    private FlowNode endNode;

    public PerformProcedureFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.PerformStatementContext performStatement = new SyntaxIdentity<CobolParser.PerformStatementContext>(getExecutionContext()).get();
        CobolParser.PerformProcedureStatementContext performProcedureStatementContext = performStatement.performProcedureStatement();
        if (isVarying()) {
            condition = nodeService.node(performProcedureStatementContext.performType(), this, staticFrameContext);
        }

        if (performProcedureStatementContext != null) return;
        inlineStatementContext = nodeService.node(performStatement.performInlineStatement(), this, staticFrameContext);
        inlineStatementContext.buildFlow();
    }

    private boolean isVarying() {
        CobolParser.PerformStatementContext performStatement = new SyntaxIdentity<CobolParser.PerformStatementContext>(getExecutionContext()).get();
        CobolParser.PerformProcedureStatementContext performProcedureStatementContext = performStatement.performProcedureStatement();
        return performProcedureStatementContext.performType() != null && performProcedureStatementContext.performType().performVarying() != null;
    }

    @Override
    public void buildOutgoingFlow() {
        // Call super here because this is still a normal statement which will continue its normal flow, after PERFORM returns
        super.buildOutgoingFlow();
    }

    @Override
    public void buildControlFlow() {
        CobolParser.PerformStatementContext performStatement = new SyntaxIdentity<CobolParser.PerformStatementContext>(getExecutionContext()).get();
        CobolParser.PerformProcedureStatementContext performProcedureStatementContext = performStatement.performProcedureStatement();
        CobolParser.ProcedureNameContext procedureNameContext = performProcedureStatementContext.procedureName();
        String procedureName = procedureNameContext.getText();
        logger.finer("Found a PERFORM, routing to " + procedureName);
        startNode = nodeService.sectionOrParaWithName(procedureName);
        if (performStatement.performProcedureStatement().through() == null) {
            procedures.add(startNode);
            endNode = startNode;
        } else {
            CobolParser.ProcedureNameContext endProcedureNameContext = performStatement.performProcedureStatement().through().procedureName();
            endNode = nodeService.sectionOrParaWithName(endProcedureNameContext.getText());
            procedures.addAll(allProcedures(startNode, endNode));
        }
    }

    private List<FlowNode> allProcedures(FlowNode startProcedure, FlowNode endProcedure) {
        FlowNode current = startProcedure;
        List<FlowNode> allInclusiveProcedures = new ArrayList<>();
        while (current != endProcedure && !current.getOutgoingNodes().isEmpty()) {
            allInclusiveProcedures.add(current);
            current = current.getOutgoingNodes().getFirst();
        }
        allInclusiveProcedures.add(endProcedure);
        return allInclusiveProcedures;
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        if (inlineStatementContext != null) {
            visitor.visitParentChildLink(this, inlineStatementContext, new VisitContext(level), nodeService);
            inlineStatementContext.accept(visitor, level);
            return;
        }

        procedures.forEach(p -> visitor.visitControlTransfer(this, p, new VisitContext(level)));

        if (isVarying()) {
            procedures.forEach(p -> visitor.visitControlTransfer(p, condition, new VisitContext(level)));
            visitor.visitControlTransfer(condition, this, new VisitContext(level));
        }
//        visitor.visitControlTransfer(this, targetNode, new VisitContext(level));
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executePerformProcedure(this, procedures, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public String label() {
        return truncated(originalText(), 30);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.PERFORM;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.CONTROL_FLOW);
    }

    @Override
    public List<FlowNode> astChildren() {
        return inlineStatementContext != null ? ImmutableList.of(inlineStatementContext) : ImmutableList.of();
    }

    @Override
    public List<FlowNode> callTargets() {
        return procedures;
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        CobolParser.PerformStatementContext performStatement = new SyntaxIdentity<CobolParser.PerformStatementContext>(getExecutionContext()).get();
        CobolParser.PerformProcedureStatementContext performProcedureStatementContext = performStatement.performProcedureStatement();
        nestedLoops = performProcedureStatementContext.performType() != null
                ? FlowIterationBuilder.build(performProcedureStatementContext.performType(), dataStructures)
                : ImmutableList.of();
    }
}
