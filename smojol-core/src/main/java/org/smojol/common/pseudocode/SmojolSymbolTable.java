package org.smojol.common.pseudocode;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;

import java.util.HashMap;
import java.util.Map;

public class SmojolSymbolTable {
    Map<String, SymbolReference> symbols = new HashMap<>();

    public SmojolSymbolTable(CobolDataStructure dataStructures, SymbolReferenceBuilder symbolReferenceBuilder) {
        ScopedDataStructureVisitor visitor = new SymbolTableVisitor(symbols, symbolReferenceBuilder);
        dataStructures.acceptScopedVisitor(visitor);
    }

    public SymbolReference reference(String symbolName) {
        return symbols.get(symbolName);
    }

    public void add(SymbolReference reference) {
        symbols.put(reference.id(), reference);
    }

    public SymbolReference reference(CobolParser.GeneralIdentifierContext generalIdentifierContext) {
        return null;
    }
}
