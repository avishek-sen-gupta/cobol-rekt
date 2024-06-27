package org.poc.common;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.eclipse.lsp.cobol.core.CobolDataTypes;
import org.eclipse.lsp.cobol.core.CobolDataTypesLexer;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class DataTypeParserTest {
    @Test
    public void canParseNumericDataLayouts() {
        String input = "S9(34)9999(1)V999(23)99PP";
        CobolDataTypesLexer antlrLexer = new CobolDataTypesLexer(CharStreams.fromString(input));
        antlrLexer.removeErrorListeners();
        CommonTokenStream tokenStream = new CommonTokenStream(antlrLexer);
//        antlrLexer.addErrorListener(listener);
        CobolDataTypes antlrParser = new CobolDataTypes(tokenStream);
        antlrParser.removeErrorListeners();
        CobolDataTypes.StartRuleContext startRuleContext = antlrParser.startRule();
//        antlrParser.addErrorListener(listener);
//        antlrParser.setErrorHandler(errorStrategy);
//        antlrParser.addParseListener(treeListener);
        assertNotNull(startRuleContext.dataTypeSpec().fraction());
        assertNull(startRuleContext.dataTypeSpec().alphanumeric());
    }

    @Test
    public void canParseAlphanumericDataLayouts() {
        String input = "9(34)9XX9X(23)99(1)99XXX(12)9(23)99";
        CobolDataTypesLexer antlrLexer = new CobolDataTypesLexer(CharStreams.fromString(input));
        antlrLexer.removeErrorListeners();
        CommonTokenStream tokenStream = new CommonTokenStream(antlrLexer);
//        antlrLexer.addErrorListener(listener);
        CobolDataTypes antlrParser = new CobolDataTypes(tokenStream);
        antlrParser.removeErrorListeners();
        CobolDataTypes.StartRuleContext startRuleContext = antlrParser.startRule();
//        antlrParser.addErrorListener(listener);
//        antlrParser.setErrorHandler(errorStrategy);
//        antlrParser.addParseListener(treeListener);
        assertNull(startRuleContext.dataTypeSpec().fraction());
        assertNotNull(startRuleContext.dataTypeSpec().alphanumeric());
    }
}
