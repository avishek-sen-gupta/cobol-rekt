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
  private final Map<String, List<Map.Entry<List<String>, SymbolReference>>> qualifiedIndex
      = new HashMap<>();

  public SmojolSymbolTable(
      CobolDataStructure dataStructures, SymbolReferenceBuilder symbolReferenceBuilder) {
    // Build legacy bare-name map (unchanged behaviour)
    ScopedDataStructureVisitor legacyVisitor =
        new SymbolTableVisitor(symbols, symbolReferenceBuilder);
    dataStructures.acceptScopedVisitor(legacyVisitor);

    // Build qualified index
    ScopedDataStructureVisitor qualifiedVisitor =
        new QualifiedPathIndexVisitor(qualifiedIndex, symbolReferenceBuilder, List.of());
    dataStructures.acceptScopedVisitor(qualifiedVisitor);
  }

  public SymbolReference reference(String symbolName) {
    return symbols.get(symbolName);
  }

  public void add(SymbolReference reference) {
    symbols.put(reference.id(), reference);
  }

  public SymbolReference reference(CobolParser.GeneralIdentifierContext ctx) {
    QualifiedName qualifiedName = extractQualifiedName(ctx);
    return resolveQualified(qualifiedName);
  }

  private SymbolReference resolveQualified(QualifiedName qualifiedName) {
    var candidates = qualifiedIndex.getOrDefault(qualifiedName.bareName(), List.of())
        .stream()
        .filter(e -> qualifiedName.isSuffixMatchedBy(e.getKey()))
        .map(Map.Entry::getValue)
        .toList();
    if (candidates.isEmpty()) return NullSymbolReference.INSTANCE;
    if (candidates.size() == 1) return candidates.get(0);
    throw new AmbiguousQualifierException(qualifiedName, candidates);
  }

  private static QualifiedName extractQualifiedName(
      CobolParser.GeneralIdentifierContext ctx) {
    var qualifiedDataName = ctx.qualifiedDataName();
    if (qualifiedDataName == null) {
      return QualifiedName.of("__NONVARIABLE__");
    }
    var bareName = qualifiedDataName.variableUsageName().getText();
    var qualifiers = qualifiedDataName.inData().stream()
        .map(inData -> inData.variableUsageName().getText())
        .toList();
    return QualifiedName.of(bareName, qualifiers);
  }
}
