package org.smojol.common.vm.reference;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.memory.NullMemoryLayout;
import org.smojol.common.vm.structure.AccessChain;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.NamingScheme;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;
import java.util.function.Function;

public class DetachedDataStructure extends CobolDataStructure {
    private final TypedRecord value;

    public DetachedDataStructure(TypedRecord value) {
        super(value.toString(), -1, CobolDataType.DETACHED, "[DETACHED]");
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
        return List.of();
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
    public void set(String destinationRecordID, CobolReference ref) {

    }

    @Override
    public void reset(String recordID) {

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
    public void add(String recordID, CobolReference ref) {

    }

    @Override
    public void subtract(String recordID, CobolReference ref) {

    }

    @Override
    public void multiply(String recordID, CobolReference ref) {

    }

    @Override
    public void divide(String recordID, CobolReference ref) {

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
