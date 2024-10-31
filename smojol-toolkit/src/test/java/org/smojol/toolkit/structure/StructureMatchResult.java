package org.smojol.toolkit.structure;

import java.util.List;

public record StructureMatchResult(SelfStructureMatchResult selfMatchResult, List<StructureMatchResult> childResults) {
    public boolean selfMatched() {
        return selfMatchErrors().isEmpty();
    }

    public List<StructurePropertyMatchResult> selfMatchErrors() {
        return selfMatchResult.propertyMatchResults().stream().filter(pmr -> !pmr.matched()).toList();
    }

    public List<String> selfMatchErrorMessages() {
        return selfMatchErrors().stream().map(StructurePropertyMatchResult::message).toList();
    }
}
