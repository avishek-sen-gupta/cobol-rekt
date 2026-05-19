package org.smojol.common.pseudocode;

import java.util.List;

public class AmbiguousQualifierException extends RuntimeException {
  private final QualifiedName qualifiedName;
  private final List<?> candidates;

  public AmbiguousQualifierException(QualifiedName qualifiedName, List<?> candidates) {
    super(
        "Ambiguous reference to '"
            + qualifiedName.bareName()
            + "': "
            + candidates.size()
            + " candidates match qualifier "
            + qualifiedName.parts());
    this.qualifiedName = qualifiedName;
    this.candidates = candidates;
  }

  public QualifiedName qualifiedName() {
    return qualifiedName;
  }

  public List<?> candidates() {
    return candidates;
  }
}
