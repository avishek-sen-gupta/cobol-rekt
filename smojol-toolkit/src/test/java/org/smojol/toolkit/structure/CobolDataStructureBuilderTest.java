package org.smojol.toolkit.structure;

import org.junit.jupiter.api.Test;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.task.TestTaskRunner;
import org.smojol.toolkit.interpreter.structure.DefaultFormat1DataStructureBuilder;
import org.smojol.toolkit.interpreter.structure.TableDataStructure;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultError;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class CobolDataStructureBuilderTest {
    @Test
    public void testCanBuildBasicPrimitiveStructure() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("data-structures.cbl", "test-code/structure")
                .runTask2(CommandLineAnalysisTask.DO_NOTHING, new DefaultFormat1DataStructureBuilder());
        BaseAnalysisModel model = switch (taskResult) {
            case AnalysisTaskResultError e -> fail(e.getException());
            case AnalysisTaskResultOK o -> o.getDetail();
        };
        CobolDataStructure dsRoot = model.dataStructures();
        List<CobolDataStructure> structs = dsRoot.subStructures();
        assertEquals(18, structs.size());
        assertFormat1RecordDetail(structs.getFirst(), "EXCHANGE-PART-01", AbstractCobolType.STRING, false);
        assertTableDetail(structs.get(1), "SOME-ARRAY", AbstractCobolType.STRING, 10, false);
    }


    private void assertTableDetail(CobolDataStructure structure, String tableName, AbstractCobolType tableElementType, int occurrences, boolean isRedefinition) {
        assertTrue(structure instanceof TableDataStructure);
        TableDataStructure typed = (TableDataStructure) structure;
        assertEquals(tableName, typed.name());
        List<CobolDataStructure> elements = typed.subStructures();
        assertEquals(occurrences, elements.size());
        assertFalse(elements.isEmpty());
        elements.forEach(e -> {
            assertEquals(tableName, e.name());
            assertEquals(tableElementType, e.getDataType().abstractType());
            assertEquals(isRedefinition, e.isRedefinition());
        });
    }

    private static void assertFormat1RecordDetail(CobolDataStructure recordDefinition, String recordName, AbstractCobolType abstractDataType, boolean isRedefinition) {
        assertTrue(recordDefinition instanceof Format1DataStructure);
        Format1DataStructure typed = (Format1DataStructure) recordDefinition;
        assertEquals(isRedefinition, typed.isRedefinition());
        assertEquals(recordName, typed.name());
        assertEquals(abstractDataType, typed.getDataType().abstractType());
    }
}
