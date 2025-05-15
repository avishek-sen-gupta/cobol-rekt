package com.mojo.algorithms.graph;

public record NodeDFSStatistics(int treeDepth, int discoveryStartTime, int discoveryEndTime) {
    public boolean isInitialised() {
        return discoveryStartTime != -1;
    }
}
