package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.smojol.common.id.Identifiable;
import org.smojol.common.transpiler.TranspilerFlowgraph;

import java.util.*;
import java.util.stream.Collectors;

public class RemoveGotoTask {

    private final TranspilerFlowgraph flowgraph;

    public RemoveGotoTask(TranspilerFlowgraph flowgraph) {
        this.flowgraph = flowgraph;
    }

    public void run() {
    }
}
