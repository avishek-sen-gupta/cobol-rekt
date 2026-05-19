package org.smojol.common.vm.structure;

import com.mojo.algorithms.domain.TypedRecord;
import com.mojo.algorithms.types.CobolDataType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.reference.CobolReference;

/**
 * Builds minimal CobolDataStructure trees for testing. Same package as CobolDataStructure to access
 * the protected 'structures' field. Uses a minimal test stub that returns the name directly (unlike
 * NullDataStructure which wraps it in "NULL[...]").
 */
public class CobolDataStructureTestFactory {

  /** Minimal concrete CobolDataStructure that returns name() as given. */
  static class TestNode extends CobolDataStructure {
    private final String nodeName;

    TestNode(String name) {
      super(name, -1, CobolDataType.GROUP, name, SourceSection.NONE);
      this.nodeName = name;
    }

    @Override
    public String name() {
      return nodeName;
    }

    @Override
    public boolean isRedefinition() {
      return false;
    }

    @Override
    public Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme() {
      return NamingScheme.NULL;
    }

    @Override
    public String content() {
      return nodeName;
    }

    @Override
    public MemoryLayout layout() {
      return null;
    }

    @Override
    public List<CobolDataStructure> matches(String recordID) {
      return nodeName.equalsIgnoreCase(recordID) ? List.of(this) : List.of();
    }

    @Override
    public CobolDataStructure addConditionalVariable(
        ConditionalDataStructure conditionalDataStructure) {
      return this;
    }

    public TestNode copy(
        Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
      return this;
    }

    @Override
    public void set(CobolReference ref) {}

    @Override
    public void reset() {}

    @Override
    public TypedRecord getValue() {
      return TypedRecord.NULL;
    }

    @Override
    public CobolDataStructure cobolIndex(int index) {
      return this;
    }

    @Override
    public void add(CobolReference ref) {}

    @Override
    public void subtract(CobolReference ref) {}

    @Override
    public void multiply(CobolReference ref) {}

    @Override
    public void divide(CobolReference ref) {}

    @Override
    public int allocateLayouts(int headPointer, MemoryRegion region) {
      return 0;
    }

    @Override
    public void expandTables() {}

    @Override
    public void calculateMemoryRequirements() {}

    @Override
    public void allocateRecordPointers() {}

    @Override
    public boolean buildRedefinitions(CobolDataStructure root) {
      return false;
    }

    @Override
    public int size() {
      return 0;
    }

    @Override
    protected void internalSet(TypedRecord r) {}

    @Override
    protected AccessChain typeSpecificChain(String subRecordID, AccessChain chain) {
      return chain;
    }
  }

  /** Create a leaf node with the given name. */
  public static CobolDataStructure leaf(String name) {
    return new TestNode(name);
  }

  /** Create a node with the given name and attach children to it. */
  public static CobolDataStructure node(String name, CobolDataStructure... children) {
    CobolDataStructure n = new TestNode(name);
    n.structures = new ArrayList<>(Arrays.asList(children));
    return n;
  }
}
