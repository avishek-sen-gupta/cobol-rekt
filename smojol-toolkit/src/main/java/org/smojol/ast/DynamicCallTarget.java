package org.smojol.ast;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;

public class DynamicCallTarget extends CallTarget {
    private CobolParser.GeneralIdentifierContext cobolIdentifier;
    private IdmsParser.GeneralIdentifierContext idmsIdentifier;

    public DynamicCallTarget(CobolParser.GeneralIdentifierContext identifier) {
        super(identifier.getText(), ReferenceType.DYNAMIC);
        this.cobolIdentifier = identifier;
    }

    public DynamicCallTarget(IdmsParser.GeneralIdentifierContext identifier) {
        super(identifier.getText(), ReferenceType.DYNAMIC);
        idmsIdentifier = identifier;
    }
}
