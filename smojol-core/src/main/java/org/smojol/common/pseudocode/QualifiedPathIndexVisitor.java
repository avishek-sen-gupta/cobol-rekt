package org.smojol.common.pseudocode;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Scoped visitor that builds a bare-name -> [(fullPath, SymbolReference)] index.
 * Each child invocation returns a new visitor with the extended path.
 * The index map is shared across all visitors (accumulated during traversal).
 * Reuses SymbolReference objects from the already-built bare-name symbols map to avoid
 * creating duplicate SymbolReference instances for the same node.
 */
class QualifiedPathIndexVisitor implements ScopedDataStructureVisitor {

    private final Map<String, List<Map.Entry<List<String>, SymbolReference>>> index;
    private final Map<String, SymbolReference> symbols;
    private final List<String> currentPath;

    QualifiedPathIndexVisitor(
            Map<String, List<Map.Entry<List<String>, SymbolReference>>> index,
            Map<String, SymbolReference> symbols,
            List<String> currentPath) {
        this.index = index;
        this.symbols = symbols;
        this.currentPath = currentPath;
    }

    @Override
    public ScopedDataStructureVisitor visit(CobolDataStructure data) {
        var childPath = Stream.concat(currentPath.stream(), Stream.of(data.name())).toList();
        // Reuse the SymbolReference already built by the first (legacy) traversal
        var ref = symbols.getOrDefault(data.name(), NullSymbolReference.INSTANCE);
        index.computeIfAbsent(data.name(), k -> new ArrayList<>()).add(Map.entry(childPath, ref));
        return new QualifiedPathIndexVisitor(index, symbols, childPath);
    }
}
