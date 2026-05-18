package org.smojol.toolkit.ast;

import java.util.logging.Logger;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;

public class DynamicCallTarget extends CallTarget {
  private static final Logger LOGGER = Logger.getLogger(DynamicCallTarget.class.getName());
  private CobolParser.GeneralIdentifierContext cobolIdentifier;
  private IdmsParser.GeneralIdentifierContext idmsIdentifier;

  public DynamicCallTarget(CobolParser.GeneralIdentifierContext identifier) {
    super(identifier.getText(), ProgramReferenceType.DYNAMIC);
    this.cobolIdentifier = identifier;
  }

  public DynamicCallTarget(IdmsParser.GeneralIdentifierContext identifier) {
    super(identifier.getText(), ProgramReferenceType.DYNAMIC);
    idmsIdentifier = identifier;
  }
}
