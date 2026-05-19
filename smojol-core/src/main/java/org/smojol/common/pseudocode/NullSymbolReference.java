package org.smojol.common.pseudocode;

public final class NullSymbolReference extends SymbolReference {
  public static final NullSymbolReference INSTANCE = new NullSymbolReference();

  private NullSymbolReference() {
    super("__NULL__");
  }
}
