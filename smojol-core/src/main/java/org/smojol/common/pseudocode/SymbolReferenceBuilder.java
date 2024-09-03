package org.smojol.common.pseudocode;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.id.IdProvider;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.FigurativeConstantMap;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

public class SymbolReferenceBuilder {
    private final IdProvider idProvider;

    public SymbolReferenceBuilder(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    public SymbolReference recordReference(CobolDataStructure record) {
        return new RecordSymbolReference(record, record.name());
    }

    public SymbolReference expressionSymbolReference(CobolExpression expression) {
        return new ExpressionSymbolReference(expression, idProvider.next());
    }

    public SymbolReference nullReference() {
        return new NullSymbolReference();
    }

    public SymbolReference staticReference(TypedRecord data) {
        return new StaticSymbolReference(data, idProvider.next());
    }

    public SymbolReference intermediateSymbolReference() {
        return new IntermediateSymbolReference(idProvider.next());
    }

    public SymbolReference literalReference(CobolParser.LiteralContext literal) {
        if (literal.numericLiteral() != null) return new StaticSymbolReference(TypedRecord.typedNumber(literal.getText()), idProvider.next());
        else if (literal.charString() != null) return new StaticSymbolReference(TypedRecord.typedString(literal.getText()), idProvider.next());
        else if (literal.booleanLiteral() != null) return new StaticSymbolReference(TypedRecord.typedBoolean(Boolean.valueOf(literal.getText())), idProvider.next());
        else if (literal.figurativeConstant() != null) return new StaticSymbolReference(TypedRecord.typedString(new FigurativeConstantMap().map(literal.getText())), idProvider.next());
        throw new UnsupportedOperationException("Unsupported literal type: " + literal.getText());
    }
}
