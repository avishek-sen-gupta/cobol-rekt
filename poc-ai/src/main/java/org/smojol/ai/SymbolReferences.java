package org.smojol.ai;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.util.*;

public class SymbolReferences {
    private final CobolEntityNavigator navigator;
    private final ParseTree scope;
    private Map<String, List<ParseTree>> symbolReferences = new HashMap();

    public SymbolReferences(CobolEntityNavigator navigator, ParseTree scope) {
        this.navigator = navigator;
        this.scope = scope;
    }

    public void add(String symbolName) {
        String symbol = symbolName.replace("[", "").replace("]", "").replace("/", "");
        ParseTree sectionOrParaTarget = navigator.target(symbol);
        List<ParseTree> statementTargets = navigator.statementsContaining(symbol, scope);
        List<ParseTree> allTargets = new ArrayList<>();
        allTargets.addAll(statementTargets);
//        if (!statementTargets.isEmpty()) allTargets.add(statementTargets.getLast());
        if (sectionOrParaTarget != null) allTargets.add(sectionOrParaTarget);
        symbolReferences.put(symbol, allTargets);
    }

    public List<ParseTree> get(String symbol) {
        return symbolReferences.get(symbol);
    }

    public Collection<List<ParseTree>> getSymbols() {
        return symbolReferences.values();
    }
}
