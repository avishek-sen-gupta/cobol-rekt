package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.NodeText;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.LiteralResolver;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;
import java.util.function.Function;

public class ConditionalDataStructure extends CobolDataStructure {
    private final CobolParser.DataDescriptionEntryFormat3Context dataDescription;

    public ConditionalDataStructure(CobolParser.DataDescriptionEntryFormat3Context dataDescription, CobolDataStructure parent, SourceSection sourceSection) {
        super(dataDescription.getText(), 88, parent.getDataType(), NodeText.originalText(dataDescription), sourceSection);
        this.dataDescription = dataDescription;
        this.parent = parent;
    }

    @Override
    public boolean isRedefinition() {
        return false;
    }

    @Override
    public void accept(DataStructureVisitor visitor, CobolDataStructure parent, Function<CobolDataStructure, Boolean> stopRecurseCondition, CobolDataStructure root) {
        visitor.visit(this, parent, root);
    }

    @Override
    public String name() {
        return dataDescription.entryName() != null ? dataDescription.entryName().getText() : "UNNAMED-88";
    }

    @Override
    public Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme() {
        return NamingScheme.IDENTITY;
    }

    @Override
    public String content() {
        return dataDescription.getText();
    }

    @Override
    public MemoryLayout layout() {
        throw new UnsupportedOperationException("Conditional variable cannot be queried for memory layout");
    }

    @Override
    public CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure) {
        throw new UnsupportedOperationException("Cannot add conditional variable to conditional variable");
    }

    @Override
    public CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        return this;
    }

    @Override
    public void set(CobolReference ref) {
        throw new UnsupportedOperationException("Cannot set conditional variable");
    }

    @Override
    public void reset() {
        throw new UnsupportedOperationException("Cannot reset conditional variable");
    }

    @Override
    public void add(CobolReference ref) {
        throw new UnsupportedOperationException("Cannot add to a conditional variable");
    }

    @Override
    public void subtract(CobolReference ref) {
        throw new UnsupportedOperationException("Cannot subtract from a conditional variable");
    }

    @Override
    public void multiply(CobolReference ref) {
        throw new UnsupportedOperationException("Cannot multiply a conditional variable");
    }

    @Override
    public void divide(CobolReference ref) {
        throw new UnsupportedOperationException("Cannot divide a conditional variable");
    }

    @Override
    public int allocateLayouts(int headPointer, MemoryRegion region) {
        throw new UnsupportedOperationException("Cannot allocate layout for conditional variable");
    }

    @Override
    public void expandTables() {
        throw new UnsupportedOperationException("Cannot expand tables for conditional variable");
    }

    @Override
    public void calculateMemoryRequirements() {
        throw new UnsupportedOperationException("Cannot calculate memory requirements for conditional variable");
    }

    @Override
    public void allocateRecordPointers() {
        throw new UnsupportedOperationException("Cannot allocate memory for a conditional variable");
    }

    @Override
    public boolean buildRedefinitions(CobolDataStructure root) {
        throw new UnsupportedOperationException("A conditional variable cannot have redefinitions");
    }

    @Override
    public int size() {
        throw new UnsupportedOperationException("Cannot size a conditional variable");
    }

    @Override
    public TypedRecord getValue() {
        Object actualValue = parent.getValue();
        List<CobolParser.DataValueClauseContext> permittedValues = dataDescription.dataValueClause();
        for (CobolParser.DataValueClauseContext acceptedValue : permittedValues) {
            List<CobolParser.DataValueIntervalContext> intervals = acceptedValue.dataValueClauseLiteral().dataValueInterval();
            for (CobolParser.DataValueIntervalContext intervalContext : intervals) {
                // TODO: From...to interval checking not supported yet
                // TODO: Need to remove enclosing quotes since parse doesn't remove them

                if (parent.typed(resolve(intervalContext.dataValueIntervalFrom())).equals(actualValue)) return TypedRecord.TRUE;
            }
        }
        return TypedRecord.FALSE;
    }

    @Override
    public CobolDataStructure cobolIndex(int index) {
        throw new UnsupportedOperationException("Cannot index a conditional variable");
    }

    private String resolve(CobolParser.DataValueIntervalFromContext dataValueIntervalFromContext) {
        CobolParser.LiteralContext literal = dataValueIntervalFromContext.literal();
        TypedRecord data = ((PrimitiveCobolExpression) new LiteralResolver().literal(literal, dataType.abstractType())).data();
        return ConversionStrategy.convert(data, AbstractCobolType.STRING).asString();
    }

    @Override
    protected void internalSet(TypedRecord r) {
        throw new UnsupportedOperationException("Cannot set value for a conditional variable");
    }

    @Override
    protected AccessChain typeSpecificChain(String subRecordID, AccessChain chain) {
        throw new UnsupportedOperationException("Cannot find chains in a conditional variable");
    }

    public List<CobolDataStructure> matches(String recordID) {
        if (dataDescription.entryName() == null) return ImmutableList.of();
        return dataDescription.entryName().getText().equals(recordID) ? ImmutableList.of(this) : ImmutableList.of();
    }
}
