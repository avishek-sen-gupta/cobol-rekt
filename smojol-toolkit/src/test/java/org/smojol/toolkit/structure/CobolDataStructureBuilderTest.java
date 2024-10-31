package org.smojol.toolkit.structure;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;
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
                        table("SOME-ARRAY", false, 3,
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
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any(),
                        any()
                ).match(dsRoot);
        assertNoErrors(lol);
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

    private DataStructureMatcher root(DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            ImmutableList<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(false, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch("[ROOT]", recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(AbstractCobolType.OBJECT, recordDefinition.getDataType().abstractType(), recordDefinition, "abstractType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    private DataStructureMatcher any(DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> new SelfStructureMatchResult(true, ImmutableList.of(mustBeTrue(recordDefinition != null, recordDefinition, "notNull"))), matchers.length > 0, matchers);
    }

    private static void assertFormat1RecordDetail(CobolDataStructure recordDefinition, String recordName, AbstractCobolType abstractDataType, boolean isRedefinition) {
        assertTrue(recordDefinition instanceof Format1DataStructure);
        Format1DataStructure typed = (Format1DataStructure) recordDefinition;
        assertEquals(isRedefinition, typed.isRedefinition());
        assertEquals(recordName, typed.name());
        assertEquals(abstractDataType, typed.getDataType().abstractType());
    }

    private static DataStructureMatcher format1(String recordName, AbstractCobolType abstractDataType, boolean isRedefinition, DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            ImmutableList<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustBeTrue(recordDefinition instanceof Format1DataStructure, recordDefinition, "type"),
                    mustMatch(isRedefinition, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(abstractDataType, recordDefinition.getDataType().abstractType(), recordDefinition, "abstractType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    private static DataStructureMatcher group(String recordName, DataStructureMatcher... matchers) {
        return group(recordName, false, matchers);
    }

    private static DataStructureMatcher group(String recordName, boolean isRedefinition, DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            ImmutableList<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustBeTrue(recordDefinition instanceof Format1DataStructure, recordDefinition, "type"),
                    mustMatch(CobolDataType.GROUP, recordDefinition.getDataType(), recordDefinition, "isGroup"),
                    mustMatch(isRedefinition, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(AbstractCobolType.OBJECT, recordDefinition.getDataType().abstractType(), recordDefinition, "abstractType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    private static DataStructureMatcher format1(String recordName, AbstractCobolType abstractDataType, DataStructureMatcher... matchers) {
        return format1(recordName, abstractDataType, false, matchers);
    }

    private static DataStructureMatcher string(String recordName) {
        return string(recordName, false);
    }

    private static DataStructureMatcher string(String recordName, boolean isRedefinition) {
        return format1(recordName, AbstractCobolType.STRING, isRedefinition);
    }

    private static DataStructureMatcher number(String recordName) {
        return number(recordName, false);
    }

    private static DataStructureMatcher number(String recordName, boolean isRedefinition) {
        return format1(recordName, AbstractCobolType.NUMBER, isRedefinition);
    }

    private static StructurePropertyMatchResult mustBeTrue(boolean condition, CobolDataStructure recordDefinition, String conditionDescription) {
        return condition ? new StructurePropertyMatchResult(true, "OK") :
                new StructurePropertyMatchResult(false, String.format("Expected condition '%s' on record '%s' to be true but was false", conditionDescription, recordDefinition.name()));
    }

    private static StructurePropertyMatchResult mustMatch(Object expected, Object actual, CobolDataStructure recordDefinition, String field) {
        boolean matched = expected.equals(actual);
        return new StructurePropertyMatchResult(matched, matched ? "OK" : String.format("Expected field '%s' of record '%s' to be %s but was %s", field, recordDefinition.name(), expected, actual));
    }

    private static DataStructureMatcher table(String recordName, boolean isRedefinition, int occurrences, DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            ImmutableList<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustBeTrue(recordDefinition instanceof TableDataStructure, recordDefinition, "typeMustBeTable"),
                    mustMatch(isRedefinition, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(occurrences, recordDefinition.subStructures().size(), recordDefinition, "elementSize"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    private static void assertRoot(CobolDataStructure recordDefinition) {
        assertTrue(recordDefinition instanceof Format1DataStructure);
        assertEquals("[ROOT]", recordDefinition.name());
        assertFalse(recordDefinition.isRedefinition());
        assertEquals(AbstractCobolType.OBJECT, recordDefinition.getDataType().abstractType());
    }
}

