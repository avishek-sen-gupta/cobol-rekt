package org.smojol.common.vm.structure;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.memory.*;
import org.smojol.common.vm.reference.CobolReference;
import com.mojo.algorithms.types.CobolDataType;
import com.mojo.algorithms.transpiler.TypedRecord;

import java.util.List;
import java.util.function.Function;

public class StaticDataStructure extends CobolDataStructure {
    private final TypedRecord value;

    public StaticDataStructure(String name, int levelNumber, CobolDataType dataType, TypedRecord value) {
        super(name, levelNumber, dataType, "[STATIC]", SourceSection.PROCEDURE_DIVISION);
        this.value = value;
    }

    @Override
    public boolean isRedefinition() {
        return false;
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme() {
        return null;
    }

    @Override
    public String content() {
        return name;
    }

    @Override
    public MemoryLayout layout() {
        return new NullMemoryLayout();
    }

    @Override
    public List<CobolDataStructure> matches(String recordID) {
        return name.equals(recordID.trim()) ? List.of(this) : List.of();
    }

    @Override
    public CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure) {
        throw new UnsupportedOperationException("Cannot add conditional variable to static data structure");
    }

    @Override
    public CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        return new StaticDataStructure(name, levelNumber, dataType, value);
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
        return this;
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
        return new EmptyAccessChain();
    }
}
