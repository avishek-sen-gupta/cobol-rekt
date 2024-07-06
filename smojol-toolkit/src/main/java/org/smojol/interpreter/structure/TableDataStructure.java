package org.smojol.interpreter.structure;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.memory.RangeMemoryAccess;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.common.vm.structure.NamingScheme;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.GroupDataTypeSpec;

import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

public class TableDataStructure extends Format1DataStructure {
    private final int numElements;
    private int childSize;
    private CobolDataType elementType;

    public TableDataStructure(CobolParser.DataDescriptionEntryFormat1Context structure, int numElements, UnresolvedReferenceStrategy strategy) {
        super(structure, strategy, CobolDataType.TABLE);
        elementType = cobolDataType(structure);
        this.numElements = numElements;
    }

    // Copy constructor
    public TableDataStructure(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme, CobolParser.DataDescriptionEntryFormat1Context dataDescription, List<CobolDataStructure> copy, int level, CobolDataStructure parent, boolean isComposite, UnresolvedReferenceStrategy unresolvedReferenceStrategy, List<ConditionalDataStructure> conditions, int numElements) {
        super(namingScheme, dataDescription, copy, level, parent, isComposite, unresolvedReferenceStrategy, conditions, CobolDataType.TABLE);
        this.numElements = numElements;
    }

    @Override
    public void expandTables() {
        if (!isComposite) {
            structures = IntStream.range(0, numElements).mapToObj(i -> (CobolDataStructure) new Format1DataStructure(NamingScheme.INDEXED.apply(i), dataDescription, copy(structures), level(), this, isComposite, unresolvedReferenceStrategy, conditions, elementType)).toList();
        } else {
            structures.forEach(CobolDataStructure::expandTables);
            structures = IntStream.range(0, numElements).mapToObj(i -> copy(NamingScheme.INDEXED.apply(i))).toList();
//            List<List<CobolDataStructure>> childGroups = IntStream.range(0, numElements).mapToObj(i -> structures.stream().map(cobolDataStructure -> cobolDataStructure.copy(NamingScheme.INDEXED)).toList()).toList();
//            structures = childGroups.stream().flatMap(List::stream).toList();
        }
    }

    @Override
    public CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme) {
        return new TableDataStructure(namingScheme, dataDescription, copy(structures), level(), parent, isComposite, unresolvedReferenceStrategy, conditions, numElements);
    }

    @Override
    public CobolDataStructure index(int index) {
        return structures.get(index);
    }

    @Override
    public void calculateMemoryRequirements() {
        structures.forEach(CobolDataStructure::calculateMemoryRequirements);
        Integer groupSize = primaryDefinitions().stream().map(CobolDataStructure::size).reduce(0, Integer::sum);
        typeSpec = new ImmutablePair<>(new GroupDataTypeSpec(groupSize), groupSize);
        childSize = groupSize / numElements;
    }

    @Override
    public int allocateLayouts(int headPointer, MemoryRegion region) {
        RangeMemoryAccess access = new RangeMemoryAccess(region, headPointer, headPointer + size() - 1);
        layout = new MemoryLayout(access, typeSpec.getLeft());
        int internalHeadPointer = headPointer;
        for (CobolDataStructure structure : primaryDefinitions()) {
            internalHeadPointer = structure.allocateLayouts(internalHeadPointer, region);
        }
        return internalHeadPointer;
    }
}
