package org.smojol.toolkit.interpreter.structure;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.memory.RangeMemoryAccess;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;
import org.smojol.common.vm.structure.*;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.GroupDataTypeSpec;

import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

public class TableDataStructure extends Format1DataStructure {
    private final int numElements;
    private int childSize;
    private CobolDataType elementType;

    public TableDataStructure(CobolParser.DataDescriptionEntryFormat1Context structure, int numElements, UnresolvedReferenceStrategy strategy, SourceSection sourceSection) {
        super(structure, strategy, sourceSection);
        elementType = cobolDataType(structure);
        this.numElements = numElements;
    }

    // Copy constructor
//    public TableDataStructure(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme, CobolParser.DataDescriptionEntryFormat1Context dataDescription, List<CobolDataStructure> childStructures, int level, CobolDataStructure parent, boolean isComposite, UnresolvedReferenceStrategy unresolvedReferenceStrategy, List<ConditionalDataStructure> conditions, int numElements, SourceSection sourceSection) {
//        super(namingScheme, dataDescription, childStructures, level, parent, isComposite, unresolvedReferenceStrategy, conditions, CobolDataType.TABLE, sourceSection);
//        this.numElements = numElements;
//    }

    // Copy constructor
    public TableDataStructure(TableDataStructure that, Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        super(that, namingScheme);
        this.numElements = that.numElements;
    }

    @Override
    public void expandTables() {
        if (!isComposite) {
//            structures = IntStream.range(0, numElements).mapToObj(i -> (CobolDataStructure) new Format1DataStructure(NamingScheme.INDEXED.apply(i), dataDescription, copy(structures), level(), this, isComposite, unresolvedReferenceStrategy, conditions, elementType, sourceSection)).toList();
            structures = IntStream.range(0, numElements).mapToObj(i -> (CobolDataStructure) new Format1DataStructure(this, NamingScheme.INDEXED.apply(i))).toList();
        } else {
            structures.forEach(CobolDataStructure::expandTables);
            structures = IntStream.range(0, numElements).mapToObj(i -> {
                List<CobolDataStructure> copiedStructures = structures.stream().map(s -> s.copy(NamingScheme.IDENTITY)).toList();
                return (CobolDataStructure) new Format1DataStructure(this, copiedStructures, NamingScheme.IDENTITY);
            }).toList();
        }
    }

    @Override
    public CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        return new TableDataStructure(this, namingScheme);
//        return new TableDataStructure(namingScheme, dataDescription, copy(structures), level(), this, isComposite, unresolvedReferenceStrategy, conditions, numElements, sourceSection);
//        return new Format1DataStructure(namingScheme, dataDescription, copy(structures), level(), this, isComposite, unresolvedReferenceStrategy, conditions, elementType);
    }

    @Override
    public CobolDataStructure cobolIndex(int index) {
        return index(index - 1);
    }

    @Override
    protected List<CobolDataStructure> primaryDefinitions() {
        return structures;
    }

    @Override
    public void calculateMemoryRequirements() {
        structures.forEach(CobolDataStructure::calculateMemoryRequirements);
        Integer groupSize = structures.stream().map(CobolDataStructure::size).reduce(0, Integer::sum);
        typeSpec = new ImmutablePair<>(new GroupDataTypeSpec(groupSize), groupSize);
        childSize = groupSize / numElements;
    }

    @Override
    protected AccessChain typeSpecificChain(String subRecordID, AccessChain chain) {
        return chain(subRecordID, structures.getFirst(), chain.curriedIndex());
    }

    @Override
    public int allocateLayouts(int headPointer, MemoryRegion region) {
        RangeMemoryAccess access = new RangeMemoryAccess(region, headPointer, headPointer + size() - 1);
        layout = new MemoryLayout(access, typeSpec.getLeft());
        int internalHeadPointer = headPointer;
        for (CobolDataStructure structure : primaryDefinitions()) {
            internalHeadPointer = structure.allocateLayouts(internalHeadPointer, region);
        }
        return headPointer + size();
    }

    @Override
    public boolean buildRedefinitions(CobolDataStructure root) {
        if (structures.getFirst().layout() != null) return false;
        if (dataDescription.dataRedefinesClause().isEmpty()) return false;
        CobolDataStructure redefinedRecord = root.reference(dataDescription.dataRedefinesClause().getFirst().dataName().getText());
        MemoryAccess originalAccess = redefinedRecord.layout().getAccess();
        int headPointer = originalAccess.fromIndex();
        allocateLayouts(headPointer, originalAccess.fullMemory());
        return true;
    }
}
