package org.smojol.common.ast;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class NodeText {
    public static String PASSTHROUGH(String text) {
        return text;
    }

    public static String originalText(ParseTree astNode) {
        return originalText(astNode, NodeText::PASSTHROUGH);
    }

    public static String idmsOriginalText(ParseTree astNode, FlowNodeService nodeService) {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        ParseTree dialectGuidContext = navigator.findByCondition(astNode, t -> t.getClass() == CobolParser.DialectGuidContext.class);
        String guid = dialectGuidContext.getText();

        ParseTree idmsTextNode = PersistentData.getDialectNode("IDMS-" + guid);
        return NodeText.originalText(idmsTextNode, NodeText::PASSTHROUGH);

    }

    public static String originalText(ParseTree astNode, Function<String, String> substitutionStrategy) {
        Token startToken = (astNode instanceof TerminalNode) ? ((TerminalNode) astNode).getSymbol() : ((ParserRuleContext) astNode).start;
        Token stopToken = (astNode instanceof TerminalNode) ? ((TerminalNode) astNode).getSymbol() : ((ParserRuleContext) astNode).stop;

        if (startToken == null) return astNode.getText();
        CharStream cs = startToken.getInputStream();
        int stopIndex = stopToken != null ? stopToken.getStopIndex() : -1;
        if (cs == null) {
            return astNode.getText();
        }
        Interval interval = new Interval(startToken.getStartIndex(), stopIndex);
        if (interval.a == -1 || interval.b == -1) {
            return astNode.getText();
        }
        return stopIndex >= startToken.getStartIndex() ? dialectInlined(cs.getText(interval), substitutionStrategy) : "<NULL>";
    }

    private static String dialectInlined(String text, Function<String, String> substitutionStrategy) {
        List<String> allDialectPlaceholders = new ArrayList<>();
        Pattern pattern = Pattern.compile("(_DIALECT_ [0-9]+)");
        Matcher matcher = pattern.matcher(text);
        return matcher.replaceAll(r -> substitutionStrategy.apply(r.group()));
    }

    public static String formatted(String s) {
        return s.replace("\n", " ").replace("\t", " ").trim();
    }
}
