package org.smojol.toolkit.examples.architecture;

public record RawSystemConnectionEntry(String currentSystemName, String adjacentSystemName,
                                       ConnectionDirection connectionDirection, SystemType systemType,
                                       ConnectionNature connectionNature, Latency latency, UsagePattern usagePattern) {
    @Override
    public String toString() {
        return String.format("CURRENT_SYSTEM=%s, ADJACENT_SYSTEM=%s, CONNECTION_DIRECTION=%s, SYSTEM_TYPE=%s, CONNECTION_NATURE=%s, LATENCY=%s, USAGE_PATTERN=%s",
                currentSystemName, adjacentSystemName, connectionDirection, systemType, connectionNature, latency, usagePattern);
    }
}
