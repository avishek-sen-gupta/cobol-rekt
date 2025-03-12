package org.smojol.toolkit.examples.architecture;

import java.util.List;
import java.util.Objects;

public record LegacySystem(String systemName, List<SystemType> technologies, String clusterName) {
    @Override
    public boolean equals(Object o) {
        if (!(o instanceof LegacySystem that)) return false;
        return Objects.equals(systemName, that.systemName);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(systemName);
    }

    @Override
    public String toString() {
        return systemName;
    }
}
