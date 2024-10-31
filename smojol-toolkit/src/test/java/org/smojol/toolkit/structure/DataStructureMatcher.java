package org.smojol.toolkit.structure;

import com.google.common.collect.ImmutableList;
import io.vavr.Function1;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.common.vm.structure.StaticDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.toolkit.interpreter.structure.TableDataStructure;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static com.google.common.collect.Streams.zip;

public class DataStructureMatcher {
    public final Function1<CobolDataStructure, SelfStructureMatchResult> selfMatcher;
    public final boolean matchChildren;
    public final List<DataStructureMatcher> childMatchers;

    DataStructureMatcher(Function1<CobolDataStructure, SelfStructureMatchResult> selfMatcher, DataStructureMatcher... childMatchers) {
        this(selfMatcher, true, childMatchers);
    }

    DataStructureMatcher(Function1<CobolDataStructure, SelfStructureMatchResult> selfMatcher, boolean matchChildren, DataStructureMatcher... childMatchers) {
        this.selfMatcher = selfMatcher;
        this.matchChildren = matchChildren;
        this.childMatchers = Arrays.asList(childMatchers);
    }

    public StructureMatchResult match(CobolDataStructure rec) {
        SelfStructureMatchResult selfMatchResult = selfMatcher.apply(rec);
        if (!selfMatchResult.matched()) return new StructureMatchResult(selfMatchResult, ImmutableList.of());
        if (!matchChildren) return new StructureMatchResult(selfMatchResult, ImmutableList.of());
        if (childMatchers.size() != rec.subStructures().size())
            return new StructureMatchResult(new SelfStructureMatchResult(false, ImmutableList.of(new StructurePropertyMatchResult(false, String.format("Sizes of matcher (%s) and structures (%s) in record '%s' do not match!", childMatchers.size(), rec.subStructures().size(), rec.name())))), ImmutableList.of());
        Stream<ImmutablePair<DataStructureMatcher, CobolDataStructure>> matcherNodePairs = zip(childMatchers.stream(), rec.subStructures().stream(), ImmutablePair::of);
        return new StructureMatchResult(selfMatchResult, matcherNodePairs.map(p -> p.getLeft().match(p.getRight())).toList());
    }

    public static DataStructureMatcher conditional(String conditionalName, CobolDataType dataType) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(ConditionalDataStructure.class, recordDefinition.getClass(), recordDefinition, "staticStructure"),
                    mustMatch(false, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(conditionalName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(dataType, recordDefinition.getDataType(), recordDefinition, "dataType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        });
    }

    public static DataStructureMatcher static_(String recordName, CobolDataType dataType) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(StaticDataStructure.class, recordDefinition.getClass(), recordDefinition, "staticStructure"),
                    mustMatch(false, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(dataType, recordDefinition.getDataType(), recordDefinition, "dataType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        });
    }

    public static DataStructureMatcher root(DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(false, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch("[ROOT]", recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(AbstractCobolType.OBJECT, recordDefinition.getDataType().abstractType(), recordDefinition, "abstractType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    public DataStructureMatcher any(DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> new SelfStructureMatchResult(true, ImmutableList.of(mustBeTrue(recordDefinition != null, recordDefinition, "notNull"))), matchers.length > 0, matchers);
    }

    public static DataStructureMatcher format1(String recordName, AbstractCobolType abstractDataType, boolean isRedefinition, DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(Format1DataStructure.class, recordDefinition.getClass(), recordDefinition, "type"),
                    mustMatch(isRedefinition, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(abstractDataType, recordDefinition.getDataType().abstractType(), recordDefinition, "abstractType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    public static DataStructureMatcher group(String recordName, DataStructureMatcher... matchers) {
        return group(recordName, false, matchers);
    }

    public static DataStructureMatcher group(String recordName, boolean isRedefinition, DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            List<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(Format1DataStructure.class, recordDefinition.getClass(), recordDefinition, "type"),
                    mustMatch(CobolDataType.GROUP, recordDefinition.getDataType(), recordDefinition, "isGroup"),
                    mustMatch(isRedefinition, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(AbstractCobolType.OBJECT, recordDefinition.getDataType().abstractType(), recordDefinition, "abstractType"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }

    public static DataStructureMatcher format1(String recordName, AbstractCobolType abstractDataType, DataStructureMatcher... matchers) {
        return format1(recordName, abstractDataType, false, matchers);
    }

    public static DataStructureMatcher string(String recordName) {
        return string(recordName, false);
    }

    public static DataStructureMatcher string(String recordName, boolean isRedefinition) {
        return format1(recordName, AbstractCobolType.STRING, isRedefinition);
    }

    public static DataStructureMatcher number(String recordName) {
        return number(recordName, false);
    }

    public static DataStructureMatcher number(String recordName, boolean isRedefinition) {
        return format1(recordName, AbstractCobolType.NUMBER, isRedefinition);
    }

    public static StructurePropertyMatchResult mustBeTrue(boolean condition, CobolDataStructure recordDefinition, String conditionDescription) {
        return condition ? new StructurePropertyMatchResult(true, "OK") :
                new StructurePropertyMatchResult(false, String.format("Expected condition '%s' on record '%s' to be true but was false", conditionDescription, recordDefinition.name()));
    }

    public static StructurePropertyMatchResult mustMatch(Object expected, Object actual, CobolDataStructure recordDefinition, String field) {
        boolean matched = expected.equals(actual);
        return new StructurePropertyMatchResult(matched, matched ? "OK" : String.format("Expected field '%s' of record '%s' to be %s but was %s", field, recordDefinition.name(), expected, actual));
    }

    public static DataStructureMatcher table(String recordName, int occurrences, DataStructureMatcher... matchers) {
        return table(recordName, occurrences, false, matchers);
    }

    public static DataStructureMatcher table(String recordName, int occurrences, boolean isRedefinition, DataStructureMatcher... matchers) {
        return new DataStructureMatcher(recordDefinition -> {
            ImmutableList<StructurePropertyMatchResult> structurePropertyMatchResults = ImmutableList.of(
                    mustMatch(TableDataStructure.class, recordDefinition.getClass(), recordDefinition, "typeMustBeTable"),
                    mustMatch(isRedefinition, recordDefinition.isRedefinition(), recordDefinition, "isRedefinition"),
                    mustMatch(recordName, recordDefinition.name(), recordDefinition, "name"),
                    mustMatch(occurrences, recordDefinition.subStructures().size(), recordDefinition, "elementSize"));
            return new SelfStructureMatchResult(true, structurePropertyMatchResults);
        }, matchers);
    }
}
