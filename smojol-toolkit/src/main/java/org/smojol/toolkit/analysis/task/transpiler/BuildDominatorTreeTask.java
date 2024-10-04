package org.smojol.toolkit.analysis.task.transpiler;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.graph.DominatorTree;
import org.smojol.common.id.Identifiable;

import java.util.*;
import java.util.logging.Logger;

/*
Algorithm based on the paper 'Graph-Theoretic Constructs for Program Control Flow Analysis' by Allen and Cocke (1972)
 */
public class BuildDominatorTreeTask {
    private static final Logger LOGGER = Logger.getLogger(BuildDominatorTreeTask.class.getName());
    private final List<Pair<Identifiable, Identifiable>> immediateDominators;
    private final Identifiable root;


    public BuildDominatorTreeTask(List<Pair<Identifiable, Identifiable>> immediateDominators, Identifiable root) {
        this.immediateDominators = immediateDominators;
        this.root = root;
    }

    public DominatorTree run() {
        Graph<Identifiable, DefaultEdge> dominatorTree = new DefaultDirectedGraph<>(DefaultEdge.class);
        immediateDominators.forEach(pair -> dominatorTree.addVertex(pair.getLeft()));
        immediateDominators.forEach(pair -> dominatorTree.addEdge(pair.getLeft(), pair.getRight()));
        return new DominatorTree(root, dominatorTree);
    }
}
