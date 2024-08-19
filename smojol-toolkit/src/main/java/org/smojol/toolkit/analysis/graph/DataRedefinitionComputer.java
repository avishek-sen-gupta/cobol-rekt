package org.smojol.toolkit.analysis.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;

import java.util.Map;

public class DataRedefinitionComputer {
    public static Map.Entry<CobolDataStructure, CobolDataStructure> redefinitionPair(CobolDataStructure redefiningRecord, CobolDataStructure dataRoot) {
        if (!(redefiningRecord instanceof Format1DataStructure)) return ImmutablePair.nullPair();
        CobolParser.DataDescriptionEntryFormat1Context dataDescription = ((Format1DataStructure) redefiningRecord).getDataDescription();
        if (dataDescription == null || dataDescription.dataRedefinesClause().isEmpty()) return ImmutablePair.nullPair();
        String redefinedRecordName = dataDescription.dataRedefinesClause().getFirst().dataName().getText();
        CobolDataStructure redefineRecord = dataRoot.reference(redefinedRecordName);
        return ImmutablePair.of(redefiningRecord, redefineRecord);
    }
}
