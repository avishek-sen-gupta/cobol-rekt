package org.smojol.toolkit.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.toolkit.ast.AddFlowNode;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

import java.util.List;

public class AddOperation implements CobolOperation {
    private final AddFlowNode add;

    public AddOperation(AddFlowNode add) {
        this.add = add;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        ArithmeticExpressionVisitor visitor = new ArithmeticExpressionVisitor();
        List<CobolParser.AddFromContext> froms = add.getFroms();
        List<CobolParser.AddToContext> tos = add.getTos();
        DeepReferenceBuilder builder = new DeepReferenceBuilder();
        tos.forEach(to -> froms.forEach(from -> builder.getReference(to, cobolDataStructure).resolve().add(builder.getReference(from, cobolDataStructure))));
    }
}
