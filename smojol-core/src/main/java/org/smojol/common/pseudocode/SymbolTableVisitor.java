package org.smojol.common.pseudocode;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;

import java.util.Map;

public class SymbolTableVisitor implements ScopedDataStructureVisitor {
    private final Map<String, SymbolReference> symbols;
    private final SymbolReferenceBuilder symbolReferenceBuilder;

    public SymbolTableVisitor(Map<String, SymbolReference> symbols, SymbolReferenceBuilder symbolReferenceBuilder) {
        this.symbols = symbols;
        this.symbolReferenceBuilder = symbolReferenceBuilder;
    }

    @Override
    public ScopedDataStructureVisitor visit(CobolDataStructure record) {
        symbols.put(record.name(), symbolReferenceBuilder.recordReference(record));
        return this;
    }
}
