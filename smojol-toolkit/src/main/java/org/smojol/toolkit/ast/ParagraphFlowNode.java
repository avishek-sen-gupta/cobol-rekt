package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.IStackFrame;
import org.smojol.common.vm.stack.StackFrames;

import java.util.Optional;

public class ParagraphFlowNode extends CompositeCobolFlowNode {
    @Override
    public FlowNodeType type() {
        return FlowNodeType.PARAGRAPH;
    }

    public ParagraphFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    public FlowNode parentOrSelf() {
        Optional<IStackFrame> parent = staticFrameContext.find(f -> f.getClass() == SectionFlowNode.class);
        if (parent.isPresent()) return parent.get().callSite();
        return this;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl forwardFlowControl) {
        CobolVmSignal signal = executeInternalRoot(interpreter, nodeService);
        if (signal == CobolVmSignal.EXIT_SCOPE)
            return forwardFlowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), CobolVmSignal.CONTINUE);
        return forwardFlowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    protected CobolVmSignal continueOrAbort(CobolVmSignal defaultSignal, CobolInterpreter interpreter, FlowNodeService nodeService) {
        if (defaultSignal == CobolVmSignal.TERMINATE || defaultSignal == CobolVmSignal.EXIT_PERFORM) return defaultSignal;
        return next(defaultSignal, interpreter, nodeService);
    }

    @Override
    public String label() {
        return ((CobolParser.ParagraphContext) executionContext).paragraphDefinitionName().getText();
    }
}
