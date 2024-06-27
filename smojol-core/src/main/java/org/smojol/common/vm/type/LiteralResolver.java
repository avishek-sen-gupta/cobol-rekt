package org.smojol.common.vm.type;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.FigurativeConstantMap;
import org.smojol.common.vm.exception.UnsupportedLiteralTypeException;

public class LiteralResolver {
    private final FigurativeConstantMap figurativeConstantMap = new FigurativeConstantMap();

    public String resolvedLiteral(CobolParser.LiteralContext literal) {
        if (literal.figurativeConstant() != null) return figurativeConstantMap.map(literal.figurativeConstant().getText());
        else if (literal.booleanLiteral() != null) return literal.getText();
        else if (literal.numericLiteral() != null) return (literal.getText());
        else if (literal.NONNUMERICLITERAL() != null) return literal.getText().replace("\"", "").replace("'", "");
        throw new UnsupportedLiteralTypeException(literal.getText());
    }
}
