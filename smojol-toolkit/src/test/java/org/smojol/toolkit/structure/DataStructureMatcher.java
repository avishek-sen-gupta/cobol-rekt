package org.smojol.toolkit.structure;

import com.google.common.collect.ImmutableList;
import io.vavr.Function1;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static com.google.common.collect.Streams.zip;

public class DataStructureMatcher {
    private final Function1<CobolDataStructure, SelfStructureMatchResult> selfMatcher;
    private final boolean matchChildren;
    private final List<DataStructureMatcher> childMatchers;

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
}
