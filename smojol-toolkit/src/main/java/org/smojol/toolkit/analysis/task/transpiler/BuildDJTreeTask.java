package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.Sets;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.graph.DJTree;
import org.smojol.common.graph.DepthFirstSpanningTree;
import org.smojol.common.graph.DominatorTree;
import org.smojol.common.id.Identifiable;

import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/*
Algorithm based on the paper 'Graph-Theoretic Constructs for Program Control Flow Analysis' by Allen and Cocke (1972)
 */
public class BuildDJTreeTask<V extends Identifiable, E> {
    private static final Logger LOGGER = Logger.getLogger(BuildDJTreeTask.class.getName());
    private final DominatorTree<V, E> dominatorTree;
    private final DepthFirstSpanningTree<V, E> spanningTree;
    private final Map<V, Set<V>> allDominators;

    public BuildDJTreeTask(DominatorTree<V, E> dominatorTree, DepthFirstSpanningTree<V, E> spanningTree, Map<V, Set<V>> allDominators) {
        this.dominatorTree = dominatorTree;
        this.spanningTree = spanningTree;
        this.allDominators = allDominators;
    }

    public DJTree<V, E> run() {
        Graph<V, E> sourceGraph = spanningTree.sourceGraph();
        Graph<V, DefaultEdge> djTree = new DefaultDirectedGraph<>(DefaultEdge.class);
        Graph<V, E> dominatorGraph = dominatorTree.graph();
        sourceGraph.vertexSet().forEach(djTree::addVertex);
        dominatorGraph.edgeSet().forEach(edge -> djTree.addEdge(dominatorGraph.getEdgeSource(edge), dominatorGraph.getEdgeTarget(edge), new DominatorEdge()));
        Sets.difference(sourceGraph.edgeSet(), dominatorGraph.edgeSet()).forEach(edge -> {
            V from = sourceGraph.getEdgeSource(edge);
            V to = sourceGraph.getEdgeTarget(edge);
            if (allDominators.get(from).contains(to))
                djTree.addEdge(from, to, new BackJoinEdge());
            else djTree.addEdge(from, to, new CrossJoinEdge());
        });
//        sourceGraph.edgeSet().forEach(edge -> djTree.addEdge(sourceGraph.getEdgeSource(edge), sourceGraph.getEdgeTarget(edge), new JoinEdge()));
        // Trust me bro
        return new DJTree<>(spanningTree.sourceGraphRoot(), (Graph<V, E>) djTree);
    }
}
