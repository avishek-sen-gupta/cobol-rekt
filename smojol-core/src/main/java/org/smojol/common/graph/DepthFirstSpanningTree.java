package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

import java.util.List;

public record DepthFirstSpanningTree<V extends Identifiable, E>(List<V> depthFirstSpanningTreeOrder, V sourceGraphRoot,
                                                                Graph<V, E> sourceGraph,
                                                                Graph<CodeGraphNode<V>, DefaultEdge> dfsLabelledTree,
                                                                CodeGraphNode<V> dfsLabelledTreeRoot) {

    public List<V> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<V> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }
}
