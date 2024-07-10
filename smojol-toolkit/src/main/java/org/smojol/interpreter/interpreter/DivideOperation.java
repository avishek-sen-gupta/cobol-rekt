package org.smojol.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.ast.DivideFlowNode;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

import java.util.List;

public class DivideOperation implements CobolOperation {
    private final DivideFlowNode divide;

    public DivideOperation(DivideFlowNode divide) {
        this.divide = divide;
    }

    public void run(CobolDataStructure cobolDataStructure) {
        ArithmeticExpressionVisitor visitor = new ArithmeticExpressionVisitor();
        CobolParser.DivisorContext divisor = divide.getDivisor();
        List<CobolParser.DivideIntoContext> dividends = divide.getDividends();
        DeepReferenceBuilder builder = new DeepReferenceBuilder();
        dividends.forEach(dividend -> builder.getReference(dividend.generalIdentifier(), cobolDataStructure).resolve().divide(builder.getReference(divisor, cobolDataStructure)));
//        dividends.forEach(dividend -> cobolDataStructure.divide(dividend.generalIdentifier().getText(), builder.getReference(divisor, cobolDataStructure)));
    }
}
