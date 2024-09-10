package org.smojol.common.vm.reference;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.memory.NullMemoryLayout;
import org.smojol.common.vm.structure.AccessChain;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.NamingScheme;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;
import java.util.function.Function;

public class DetachedDataStructure extends CobolDataStructure {
    private final TypedRecord value;

    public DetachedDataStructure(TypedRecord value) {
        this(value.toString(), value, "[DETACHED]");
    }

    public DetachedDataStructure(String name, TypedRecord value) {
        this(name, value, "[UNREFERENCED]");
    }

    private DetachedDataStructure(String name, TypedRecord value, String rawText) {
        super(name, -1, value.dataType(), rawText, SourceSection.PROCEDURE_DIVISION);
        this.value = value;
    }

    @Override
    public boolean isRedefinition() {
        return false;
    }

    @Override
    public String name() {
        return value.toString();
    }

    @Override
    public Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme() {
        return NamingScheme.IDENTITY;
    }

    @Override
    public String content() {
        return "";
    }

    @Override
    public MemoryLayout layout() {
        return new NullMemoryLayout();
    }

    @Override
    public List<CobolDataStructure> matches(String recordID) {
        return name.equals(recordID) ? List.of(this) : List.of();
    }

    @Override
    public CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure) {
        return this;
    }

    @Override
    public CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        return this;
    }

    @Override
    public void set(CobolReference ref) {

    }

    @Override
    public void reset() {

    }

    @Override
    public TypedRecord getValue() {
        return value;
    }

    @Override
    public CobolDataStructure cobolIndex(int index) {
        throw new UnsupportedOperationException("Indexing is not supported for detached structures");
    }

    @Override
    public void add(CobolReference ref) {

    }

    @Override
    public void subtract(CobolReference ref) {

    }

    @Override
    public void multiply(CobolReference ref) {

    }

    @Override
    public void divide(CobolReference ref) {

    }

    @Override
    public int allocateLayouts(int headPointer, MemoryRegion region) {
        return 0;
    }

    @Override
    public void expandTables() {

    }

    @Override
    public void calculateMemoryRequirements() {

    }

    @Override
    public void allocateRecordPointers() {

    }

    @Override
    public boolean buildRedefinitions(CobolDataStructure root) {
        return false;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    protected void internalSet(TypedRecord r) {

    }

    @Override
    protected AccessChain typeSpecificChain(String subRecordID, AccessChain chain) {
        throw new UnsupportedOperationException("Type-specific chain is not supported for detached structures");
    }
}
