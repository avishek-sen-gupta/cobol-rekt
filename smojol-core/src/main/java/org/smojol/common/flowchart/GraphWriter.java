package org.smojol.common.flowchart;

import guru.nidi.graphviz.model.MutableGraph;

public interface GraphWriter {
    void process(MutableGraph g, ChartOverlay overlay);
}
