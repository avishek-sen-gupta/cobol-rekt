package org.smojol.toolkit.ast;

import com.mojo.algorithms.domain.FlowNodeType;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

public class SectionFlowNode extends CompositeCobolFlowNode {
    @Override
    public FlowNodeType type() {
        return FlowNodeType.SECTION;
    }

    public SectionFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

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
        return ((CobolParser.ProcedureSectionContext) executionContext).procedureSectionHeader().sectionName().getText();
    }
}
