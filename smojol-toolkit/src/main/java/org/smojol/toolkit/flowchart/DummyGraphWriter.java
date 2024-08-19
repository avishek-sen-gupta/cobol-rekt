package org.smojol.toolkit.flowchart;

import guru.nidi.graphviz.model.MutableGraph;
import org.smojol.common.flowchart.ChartOverlay;
import org.smojol.common.flowchart.GraphWriter;

public class DummyGraphWriter implements GraphWriter {
    @Override
    public void process(MutableGraph g, ChartOverlay overlay) {
    }
}
