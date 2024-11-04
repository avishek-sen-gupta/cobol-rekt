package org.smojol.common.analysis;

import org.smojol.common.id.Identifiable;

import java.util.Set;

public record NaturalLoopBody<V extends Identifiable>(V loopHeader, Set<V> loopNodes) {
}
