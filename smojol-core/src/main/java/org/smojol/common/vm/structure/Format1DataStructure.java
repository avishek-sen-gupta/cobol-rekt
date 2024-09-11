package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.NodeText;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.memory.*;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.PrimitiveReference;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;
import org.smojol.common.vm.type.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.logging.Logger;

public class Format1DataStructure extends CobolDataStructure {
    private static final Logger LOGGER = Logger.getLogger(Format1DataStructure.class.getName());
    protected final UnresolvedReferenceStrategy unresolvedReferenceStrategy;
    private final Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme;
    @Getter
    protected final CobolParser.DataDescriptionEntryFormat1Context dataDescription;
    protected final List<ConditionalDataStructure> conditions = new ArrayList<>();
    protected MemoryLayout layout;
    protected Pair<DataTypeSpec, Integer> typeSpec;

    @Override
    public boolean isRedefinition() {
        return dataDescription != null && !dataDescription.dataRedefinesClause().isEmpty();
    }

    @Override
    public String name() {
//        if (dataType == CobolDataType.ROOT) return "[ROOT]";
        return namingScheme.apply(dataDescription);
    }

    @Override
    public Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme() {
        return namingScheme;
    }

    @Override
    public String content() {
        return dataDescription != null ? dataDescription.getText() : "[ROOT]";
    }

    @Override
    public MemoryLayout layout() {
        return layout;
    }

    public Format1DataStructure(CobolParser.DataDescriptionEntryFormat1Context dataDescription, UnresolvedReferenceStrategy unresolvedReferenceStrategy, SourceSection sourceSection) {
        super(NamingScheme.IDENTITY.apply(dataDescription), Integer.parseInt(dataDescription.levelNumber().getText()), cobolDataType(dataDescription), NodeText.originalText(dataDescription), sourceSection);
        this.namingScheme = NamingScheme.IDENTITY;
        this.dataDescription = dataDescription;
        LOGGER.finer("Setting value for " + dataDescription.getText());
        this.unresolvedReferenceStrategy = unresolvedReferenceStrategy;
    }

    // Root constructor
    public Format1DataStructure(int levelNumber, UnresolvedReferenceStrategy unresolvedReferenceStrategy) {
        super(NamingScheme.ROOT.apply(null), levelNumber, CobolDataType.ROOT, "[ROOT]", SourceSection.ROOT);
        this.namingScheme = NamingScheme.ROOT;
        this.layout = new NullMemoryLayout();
        this.dataDescription = null;
        this.unresolvedReferenceStrategy = unresolvedReferenceStrategy;
    }

    // Copy constructor
//    public Format1DataStructure(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme, CobolParser.DataDescriptionEntryFormat1Context dataDescription, List<CobolDataStructure> childStructures, int level, CobolDataStructure parent, boolean isComposite, UnresolvedReferenceStrategy unresolvedReferenceStrategy, List<ConditionalDataStructure> conditions, CobolDataType dataType, SourceSection sourceSection) {
//        super(namingScheme.apply(dataDescription), childStructures, level, parent, isComposite, dataType, NodeText.originalText(dataDescription), sourceSection);
//        this.namingScheme = namingScheme;
//        this.dataDescription = dataDescription;
//        this.unresolvedReferenceStrategy = unresolvedReferenceStrategy;
//        this.conditions.addAll(conditions);
//    }

    // Copy constructor
    public Format1DataStructure(Format1DataStructure that, Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        this(that, that.copy(that.structures), namingScheme);
    }

    // Copy constructor
    public Format1DataStructure(Format1DataStructure that, List<CobolDataStructure> childStructures, Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        super(namingScheme.apply(that.dataDescription), childStructures, that.levelNumber, that.parent, that.isComposite, that.dataType, NodeText.originalText(that.dataDescription), that.sourceSection);
        this.dataDescription = that.dataDescription;
        this.unresolvedReferenceStrategy = that.unresolvedReferenceStrategy;
        this.conditions.addAll(that.conditions);
        this.namingScheme = namingScheme;
    }

    @Override
    public CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
//        return new Format1DataStructure(namingScheme, dataDescription, copy(structures), level(), parent, isComposite, unresolvedReferenceStrategy, conditions, dataType, sourceSection);
        return new Format1DataStructure(this, namingScheme);
    }

    protected List<CobolDataStructure> copy(List<CobolDataStructure> structures) {
        return structures.stream().map(cobolDataStructure -> cobolDataStructure.copy(cobolDataStructure.namingScheme())).toList();
    }

    @Override
    public void set(CobolReference ref) {
        ConversionStrategy.set(this, ref);
    }

    @Override
    public void reset() {
        if (dataType != CobolDataType.GROUP) set(new PrimitiveReference(dataType.defaultValue()));
        structures.forEach(CobolDataStructure::reset);
    }

    @Override
    public void add(CobolReference ref) {
        this.set(new PrimitiveReference(this.getValue().add(ref.resolveAs(AbstractCobolType.NUMBER))));
    }

    @Override
    public void subtract(CobolReference ref) {
        this.set(new PrimitiveReference(this.getValue().subtract(ref.resolveAs(AbstractCobolType.NUMBER))));
    }

    @Override
    public void multiply(CobolReference ref) {
        this.set(new PrimitiveReference(this.getValue().multiply(ref.resolveAs(AbstractCobolType.NUMBER))));
    }

    @Override
    public void divide(CobolReference ref) {
        this.set(new PrimitiveReference(this.getValue().divide(ref.resolveAs(AbstractCobolType.NUMBER))));
    }

    public CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure) {
        conditions.add(conditionalDataStructure);
        return this;
    }

    @Override
    public List<CobolDataStructure> matches(String recordID) {
//        boolean selfMatch = dataDescription != null && dataDescription.entryName().dataName().getText().equals(recordID);
        boolean selfMatch = dataDescription != null && dataDescription.entryName().getText().equals(recordID);
        Optional<ConditionalDataStructure> condition = condition(recordID);
        List<CobolDataStructure> matches = new ArrayList<>();
        matches.addAll(selfMatch ? ImmutableList.of(this) : ImmutableList.of());
        matches.addAll(condition.map(conditionalDataStructure -> ImmutableList.of(this, conditionalDataStructure)).orElseGet(ImmutableList::of));
        return matches;
    }

    @Override
    public int size() {
        return typeSpec.getRight();
    }

    @Override
    public TypedRecord getValue() {
        return new TypedRecord(layout.readFormatted(), dataType);
    }

    @Override
    public CobolDataStructure cobolIndex(int index) {
        throw new UnsupportedOperationException("Indexing is not supported for non-Table objects");
    }

    @Override
    protected void internalSet(TypedRecord r) {
        layout.set(r.value().toString());
    }

    @Override
    protected AccessChain typeSpecificChain(String subRecordID, AccessChain chain) {
        for (int i = 0; i <= structures.size() - 1; ++i) {
            AccessChain childChain = chain.copy().staticIndex(i);
            CobolDataStructure child = structures.get(i);
            AccessChain result = chain(subRecordID, child, childChain);
            if (result.getClass() == EmptyAccessChain.class) continue;
            return result;
        }
        return new EmptyAccessChain();
    }

    @Override
    public void calculateMemoryRequirements() {
        typeSpec = typeSpecForSingle();
    }

    private Pair<DataTypeSpec, Integer> typeSpecForSingle() {
        if (dataType == CobolDataType.POINTER)
            return ImmutablePair.of(new ZonedDecimalDataTypeSpec(8, 0), 8);
        else if (!isComposite)
            return new DataLayoutBuilder().size(dataDescription.dataPictureClause().getFirst().pictureString().getFirst().getText());
        else {
            structures.forEach(CobolDataStructure::calculateMemoryRequirements);
            Integer groupSize = primaryDefinitions().stream().map(CobolDataStructure::size).reduce(0, Integer::sum);
            return ImmutablePair.of(new GroupDataTypeSpec(groupSize), groupSize);
        }
    }

    protected List<CobolDataStructure> primaryDefinitions() {
        return structures.stream().filter(s -> !s.isRedefinition()).toList();
    }

    @Override
    public void allocateRecordPointers() {
        if (dataType != CobolDataType.ROOT)
            throw new UnsupportedOperationException("Allocation can only happen for ROOT");
        primaryDefinitions().forEach(s -> {
            MemoryRegion region = new MemoryRegion(s.size());
            s.allocateLayouts(0, region);
        });
    }

    @Override
    public boolean buildRedefinitions(CobolDataStructure root) {
        LOGGER.finer("Inspecting Redefinitions of " + root + "...");
        if (!isComposite && (!isRedefinition() || isInitialisedRedefinition())) return false;
        if (isComposite && (!isRedefinition() || isInitialisedRedefinition())) {
            return structures.stream().map(dataStructure -> dataStructure.buildRedefinitions(root)).reduce(false, (a, b) -> a || b);
        }

        CobolDataStructure redefinedRecord = root.reference(dataDescription.dataRedefinesClause().getFirst().dataName().getText());
        if (redefinedRecord.layout() == null) {
            LOGGER.warning(String.format("%s does not seem to have a layout defined. Have you declared this variable?", redefinedRecord.name()));
            layout = new NullMemoryLayout();
            return false;
        }
        MemoryAccess originalAccess = redefinedRecord.layout().getAccess();
        if (isComposite) {
            LOGGER.finer(String.format("Building [%s] redefinition of group record [%s]", name(), redefinedRecord.name()));
            int headPointer = originalAccess.fromIndex();
            allocateLayouts(headPointer, originalAccess.fullMemory());
            return true;
        }
        LOGGER.finer(String.format("Building [%s] redefinition of atomic record [%s]", name(), redefinedRecord.name()));
        RangeMemoryAccess redefinedAccess = originalAccess.copy(typeSpec.getLeft().sizeInBytes());
        layout = new MemoryLayout(redefinedAccess, typeSpec.getLeft());
        return true;
    }

    protected boolean isInitialisedRedefinition() {
        return isRedefinition() && layout != null;
    }

    @Override
    public int allocateLayouts(int headPointer, MemoryRegion region) {
        if (isComposite) {
            RangeMemoryAccess access = new RangeMemoryAccess(region, headPointer, headPointer + size() - 1);
            layout = new MemoryLayout(access, typeSpec.getLeft());
            int internalHeadPointer = headPointer;
            for (CobolDataStructure structure : primaryDefinitions()) {
                internalHeadPointer = structure.allocateLayouts(internalHeadPointer, region);
            }
            return internalHeadPointer;
        }
        RangeMemoryAccess access = new RangeMemoryAccess(region, headPointer, headPointer + size() - 1);
        layout = new MemoryLayout(access, typeSpec.getLeft());
        return headPointer + size();
    }

    @Override
    public void expandTables() {
        structures.forEach(CobolDataStructure::expandTables);
    }

    @Override
    public String toString() {
//        if (dataType == CobolDataType.ROOT) return "[ROOT]";
//        return dataDescription != null ? dataDescription.entryName().getText() + " " + layout.toString() : "[ROOT]";
        return namingScheme.apply(dataDescription) + " " + layout.toString();
    }

    @Override
    public void accept(DataStructureVisitor visitor, CobolDataStructure parent, Function<CobolDataStructure, Boolean> stopRecurseCondition, CobolDataStructure root) {
        super.accept(visitor, parent, stopRecurseCondition, root);
        this.conditions.forEach(c -> c.accept(visitor, this, stopRecurseCondition, root));
    }

    private Optional<ConditionalDataStructure> condition(String subRecordID) {
        return conditions.stream().filter(c -> !c.matches(subRecordID).isEmpty()).findFirst();
    }
}
