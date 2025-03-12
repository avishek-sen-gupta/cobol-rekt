package org.smojol.toolkit.examples.architecture;

public record SystemConnectionEntry(LegacySystem currentSystem, LegacySystem adjacentSystem,
                                    ConnectionDirection connectionDirection,
                                    ConnectionNature connectionNature, Latency latency, UsagePattern usagePattern) {
    @Override
    public String toString() {
        return String.format("CURRENT_SYSTEM=%s, ADJACENT_SYSTEM=%s, CONNECTION_DIRECTION=%s, CONNECTION_NATURE=%s, LATENCY=%s, USAGE_PATTERN=%s",
                currentSystem, adjacentSystem, connectionDirection, connectionNature, latency, usagePattern);
    }
}
