package org.smojol.toolkit.structure;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.common.vm.structure.StaticDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.CobolDataType;
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
import static org.smojol.toolkit.structure.DataStructureMatcher.*;

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
        assertTableDetail(structs.get(1), "SOME-ARRAY", AbstractCobolType.STRING, 3, false);
        StructureMatchResult lol =
                root(
                        string("EXCHANGE-PART-01"),
                        table("SOME-ARRAY", 3,
                                string("SOME-ARRAY"),
                                string("SOME-ARRAY"),
                                string("SOME-ARRAY")
                        ),
                        group("SOME-PART",
                                string("SOME-PART-1"),
                                string("SOME-PART-2"),
                                number("INVOICE-AMOUNT"),
                                number("VENDOR-CORRECTION")
                        ),
                        group("SOME-GROUP",
                                table("LEVEL-10-A", 3,
                                        string("LEVEL-10-A"),
                                        string("LEVEL-10-A"),
                                        string("LEVEL-10-A")
                                        ),
                                table("LEVEL-10-B", 2,
                                    group("LEVEL-10-B",
                                        table("LEVEL-20-B", 2,
                                            string("LEVEL-20-B"),
                                            string("LEVEL-20-B")
                                        )
                                    ),
                                    group("LEVEL-10-B",
                                        table("LEVEL-20-B", 2,
                                            string("LEVEL-20-B"),
                                            string("LEVEL-20-B")
                                        )
                                    )
                                ),
                                group("AA", true,
                                        number("AA1"),
                                        number("AA2")
                                )
                        ),
                        string("SOMETHING"),
                        table("SOMEFRACTION", 2,
                                number("SOMEFRACTION"),
                                number("SOMEFRACTION")),
                        group("SOME-UNION-1", true,
                                number("UNION-CHILD-1"),
                                string("UNION-CHILD-2", true)),
                        number("SOMETEXT"),
                        string("REDEF-SOMETEXT", true),
                        number("NUMERIC-SOMETEXT", true),
                        number("CENTURY"),
                        number("REDEF", true),
                        number("SCALED"),
                        number("RESULT"),
                        string("CONDI"),
                        string("SOMETHING-LINKAGE"),
                        table("SOMEFRACTION-LINKAGE", 2,
                            number("SOMEFRACTION-LINKAGE"),
                            number("SOMEFRACTION-LINKAGE")
                        ),
                        static_("WHEN-COMPILED", CobolDataType.STRING)
                ).match(dsRoot);
        assertNoErrors(lol);
    }

    private static DataStructureMatcher conditional(String conditionalName, CobolDataType dataType) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(ConditionalDataStructure.class, recordDefinition.getClass(), recordDefinition, "staticStructure"),
                    mustMatch(false, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(conditionalName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(dataType, recordDefinition.getDataType(), recordDefinition, "dataType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        });
    }

    private DataStructureMatcher static_(String recordName, CobolDataType dataType) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(StaticDataStructure.class, recordDefinition.getClass(), recordDefinition, "staticStructure"),
                    mustMatch(false, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(dataType, recordDefinition.getDataType(), recordDefinition, "dataType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        });
    }

    private void assertNoErrors(StructureMatchResult result) {
        if (!result.selfMatched()) fail(String.join("\n", result.selfMatchErrorMessages()));
        result.childResults().forEach(this::assertNoErrors);
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

