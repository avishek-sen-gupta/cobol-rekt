package org.smojol.common.vm.structure;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.common.vm.type.CobolDataType;

import java.util.List;
import java.util.function.Function;

/**
 * Only used as a placeholder for
 */
public class NullDataStructure extends CobolDataStructure {
    private final String referenceID;

    public NullDataStructure(String referenceID) {
        super(referenceID, -99, CobolDataType.NULL);
        this.referenceID = referenceID;
    }

    @Override
    protected boolean isRedefinition() {
        return false;
    }

    @Override
    public String name() {
        return "NULL";
    }

    @Override
    public Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme() {
        return NamingScheme.NULL;
    }

    @Override
    public String content() {
        return "[NULL]";
    }

    @Override
    protected MemoryLayout layout() {
        return null;
    }

    @Override
    public List<CobolDataStructure> matches(String recordID) {
        return List.of();
    }

    @Override
    public CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure) {
        return this;
    }

    public NullDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
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
    public TypedRecord getValue() {
        System.out.println(ConsoleColors.red("WARNING: NULL value being accessed. This will be an empty string! Original reference ID: " + referenceID));
        return TypedRecord.NULL;
    }

    @Override
    protected void internalSet(TypedRecord r) {

    }
}
