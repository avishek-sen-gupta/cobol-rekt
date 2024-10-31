package org.smojol.toolkit.structure;

import java.util.List;

public record SelfStructureMatchResult(boolean matched, List<StructurePropertyMatchResult> propertyMatchResults) {
}
