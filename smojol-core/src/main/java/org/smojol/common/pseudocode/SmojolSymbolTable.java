package org.smojol.common.pseudocode;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;

public class SmojolSymbolTable {

  // Legacy bare-name map — preserved for callers using reference(String)
  Map<String, SymbolReference> symbols = new HashMap<>();

  // Qualified index: bareName -> [(fullPath, SymbolReference)]
  private final Map<String, List<Map.Entry<List<String>, SymbolReference>>> qualifiedIndex =
      new HashMap<>();

  public SmojolSymbolTable(
      CobolDataStructure dataStructures, SymbolReferenceBuilder symbolReferenceBuilder) {
    // Build legacy bare-name map (unchanged behaviour)
    ScopedDataStructureVisitor legacyVisitor =
        new SymbolTableVisitor(symbols, symbolReferenceBuilder);
    dataStructures.acceptScopedVisitor(legacyVisitor);

    // Build qualified index — reuses refs from symbols map to avoid creating duplicate
    // SymbolReference objects
    ScopedDataStructureVisitor qualifiedVisitor =
        new QualifiedPathIndexVisitor(qualifiedIndex, symbols, List.of());
    dataStructures.acceptScopedVisitor(qualifiedVisitor);
  }

  public SymbolReference reference(String symbolName) {
    return symbols.getOrDefault(symbolName, NullSymbolReference.INSTANCE);
  }

  public void add(SymbolReference reference) {
    symbols.put(reference.id(), reference);
  }

  public SymbolReference reference(CobolParser.GeneralIdentifierContext ctx) {
    var qualifiedDataName = ctx.qualifiedDataName();
    if (qualifiedDataName == null) return NullSymbolReference.INSTANCE;
    return resolveQualified(extractQualifiedName(ctx));
  }

  private SymbolReference resolveQualified(QualifiedName qualifiedName) {
    var candidates =
        qualifiedIndex.getOrDefault(qualifiedName.bareName(), List.of()).stream()
            .filter(e -> qualifiedName.isSuffixMatchedBy(e.getKey()))
            .map(Map.Entry::getValue)
            .toList();
    if (candidates.isEmpty()) return NullSymbolReference.INSTANCE;
    if (candidates.size() == 1) return candidates.get(0);
    throw new AmbiguousQualifierException(qualifiedName, candidates);
  }

  // Called only after qualifiedDataName null-check in reference(GeneralIdentifierContext)
  private static QualifiedName extractQualifiedName(CobolParser.GeneralIdentifierContext ctx) {
    var qualifiedDataName = ctx.qualifiedDataName();
    var bareName = qualifiedDataName.variableUsageName().getText();
    var qualifiers =
        qualifiedDataName.inData().stream()
            .map(inData -> inData.variableUsageName().getText())
            .toList();
    return QualifiedName.of(bareName, qualifiers);
  }
}
