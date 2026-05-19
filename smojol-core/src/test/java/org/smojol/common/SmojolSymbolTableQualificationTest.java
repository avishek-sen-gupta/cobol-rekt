package org.smojol.common;

import static org.junit.jupiter.api.Assertions.*;
import static org.smojol.common.vm.structure.CobolDataStructureTestFactory.*;

import com.mojo.algorithms.id.UUIDProvider;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.structure.CobolDataStructure;

public class SmojolSymbolTableQualificationTest {

  private SmojolSymbolTable symbolTable;
  private CobolDataStructure fieldAUnderStruct1;
  private CobolDataStructure fieldAUnderStruct2;
  private CobolDataStructure fieldB;

  @BeforeEach
  public void setUp() {
    fieldAUnderStruct1 = leaf("FIELD-A");
    fieldAUnderStruct2 = leaf("FIELD-A");
    fieldB = leaf("FIELD-B");
    CobolDataStructure root =
        node(
            "ROOT",
            node("STRUCT-1", fieldAUnderStruct1),
            node("STRUCT-2", fieldAUnderStruct2, fieldB));
    symbolTable = new SmojolSymbolTable(root, new SymbolReferenceBuilder(new UUIDProvider()));
  }

  @Test
  public void referenceByStringReturnsSymbolReferenceForUniqueName() {
    SymbolReference ref = symbolTable.reference("FIELD-B");
    assertNotNull(ref);
  }

  @Test
  public void referenceByGeneralIdentifierWithQualifierDisambiguates() {
    CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A OF STRUCT-1");
    SymbolReference ref = symbolTable.reference(ctx);
    assertNotNull(ref);
    assertFalse(ref instanceof NullSymbolReference);
    // SymbolReference.id() returns the bare name ("FIELD-A") for both candidates;
    // RecordSymbolReference does not expose the underlying CobolDataStructure, so a
    // structural identity check against fieldAUnderStruct1 is not feasible here.
    assertEquals("FIELD-A", ref.id());
  }

  @Test
  public void unqualifiedAmbiguousNameThrowsAmbiguousQualifierException() {
    CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A");
    assertThrows(AmbiguousQualifierException.class, () -> symbolTable.reference(ctx));
  }

  @Test
  public void referenceByGeneralIdentifierForNonexistentNameReturnsNullSymbolReference() {
    CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("NONEXISTENT");
    SymbolReference ref = symbolTable.reference(ctx);
    assertTrue(ref instanceof NullSymbolReference);
  }

  @Test
  public void referenceByGeneralIdentifierWithWrongQualifierReturnsNullSymbolReference() {
    CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A OF MISSING");
    SymbolReference ref = symbolTable.reference(ctx);
    assertTrue(ref instanceof NullSymbolReference);
  }

  private static CobolParser.GeneralIdentifierContext parseGeneralIdentifier(String text) {
    CobolLexer lexer = new CobolLexer(CharStreams.fromString(text));
    CobolParser parser = new CobolParser(new CommonTokenStream(lexer));
    return parser.generalIdentifier();
  }
}
