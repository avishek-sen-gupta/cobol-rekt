package org.smojol.common;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.smojol.common.navigation.SectionNameExtractor;

class SectionNameExtractorTest {

    @Test
    void extractsNameFromCobolWordWhenPresent() {
        CobolParser.SectionOrParagraphContext ctx = mock(CobolParser.SectionOrParagraphContext.class);
        CobolParser.CobolWordContext cobolWord = mock(CobolParser.CobolWordContext.class);
        when(ctx.cobolWord()).thenReturn(cobolWord);
        when(cobolWord.getText()).thenReturn("MY-SECTION");

        assertEquals("MY-SECTION", new SectionNameExtractor().sectionName(ctx));
    }

    @Test
    void extractsNameFromIntegerLiteralWhenCobolWordIsNull() {
        CobolParser.SectionOrParagraphContext ctx = mock(CobolParser.SectionOrParagraphContext.class);
        when(ctx.cobolWord()).thenReturn(null);
        CobolParser.IntegerLiteralContext intLiteral = mock(CobolParser.IntegerLiteralContext.class);
        when(ctx.integerLiteral(0)).thenReturn(intLiteral);
        when(intLiteral.getText()).thenReturn("100");

        assertEquals("100", new SectionNameExtractor().sectionName(ctx));
    }
}
