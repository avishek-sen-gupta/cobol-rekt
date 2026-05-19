package org.smojol.common.vm.reference;

import static org.junit.jupiter.api.Assertions.*;
import static org.smojol.common.vm.structure.CobolDataStructureTestFactory.*;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.Test;
import org.smojol.common.vm.structure.CobolDataStructure;

/**
 * TDD test for Task 5: CobolReferenceBuilder.resolve must use qualified name resolution so that
 * OF/IN qualifiers disambiguate duplicate bare names.
 */
public class CobolReferenceBuilderQualificationTest {

  /*
   * Tree:  ROOT -> STRUCT-2 -> FIELD-A  (duplicate bare name — listed first so bare-name search hits this one)
   *                        -> FIELD-B
   *             -> STRUCT-1 -> FIELD-A
   *
   * STRUCT-2 appears before STRUCT-1 so that a bare-name search (depth-first) finds
   * fieldAUnderStruct2 first.  The qualifier "OF STRUCT-1" must steer resolution to
   * fieldAUnderStruct1 instead.
   */

  @Test
  public void resolvePicksCorrectNodeWhenQualifierPresent() {
    CobolDataStructure fieldAUnderStruct1 = leaf("FIELD-A");
    CobolDataStructure fieldAUnderStruct2 = leaf("FIELD-A");
    // STRUCT-2 is first so bare-name lookup would return fieldAUnderStruct2
    CobolDataStructure root =
        node(
            "ROOT",
            node("STRUCT-2", fieldAUnderStruct2, leaf("FIELD-B")),
            node("STRUCT-1", fieldAUnderStruct1));

    CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A OF STRUCT-1");

    CobolReferenceBuilder builder = new CobolReferenceBuilder();
    CobolDataStructure resolved = builder.resolve(ctx, root);

    assertSame(fieldAUnderStruct1, resolved);
  }

  @Test
  public void resolveByBareNameWorksForUniqueField() {
    CobolDataStructure fieldB = leaf("FIELD-B");
    CobolDataStructure root =
        node("ROOT", node("STRUCT-1", leaf("FIELD-A")), node("STRUCT-2", leaf("FIELD-A"), fieldB));

    CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-B");

    CobolReferenceBuilder builder = new CobolReferenceBuilder();
    CobolDataStructure resolved = builder.resolve(ctx, root);

    assertSame(fieldB, resolved);
    assertNotNull(resolved);
  }

  private static CobolParser.GeneralIdentifierContext parseGeneralIdentifier(String text) {
    CobolLexer lexer = new CobolLexer(CharStreams.fromString(text));
    CobolParser parser = new CobolParser(new CommonTokenStream(lexer));
    return parser.generalIdentifier();
  }
}
