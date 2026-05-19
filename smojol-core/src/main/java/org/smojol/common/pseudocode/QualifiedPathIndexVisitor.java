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
 */
class QualifiedPathIndexVisitor implements ScopedDataStructureVisitor {

    private final Map<String, List<Map.Entry<List<String>, SymbolReference>>> index;
    private final SymbolReferenceBuilder builder;
    private final List<String> currentPath;

    QualifiedPathIndexVisitor(
            Map<String, List<Map.Entry<List<String>, SymbolReference>>> index,
            SymbolReferenceBuilder builder,
            List<String> currentPath) {
        this.index = index;
        this.builder = builder;
        this.currentPath = currentPath;
    }

    @Override
    public ScopedDataStructureVisitor visit(CobolDataStructure data) {
        var childPath = Stream.concat(currentPath.stream(), Stream.of(data.name())).toList();
        var ref = builder.recordReference(data);
        index.computeIfAbsent(data.name(), k -> new ArrayList<>()).add(Map.entry(childPath, ref));
        return new QualifiedPathIndexVisitor(index, builder, childPath);
    }
}
