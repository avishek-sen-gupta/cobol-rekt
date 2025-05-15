package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.DominatorTree;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import com.mojo.algorithms.id.Identifiable;

import java.util.*;
import java.util.logging.Logger;

/*
Algorithm based on the paper 'Graph-Theoretic Constructs for Program Control Flow Analysis' by Allen and Cocke (1972)
 */
public class BuildDominatorTreeTask<V extends Identifiable, E> {
    private static final Logger LOGGER = Logger.getLogger(BuildDominatorTreeTask.class.getName());
    private final List<Pair<V, V>> immediateDominators;
    private final V root;
    private final Class<E> edgeClass;


    public BuildDominatorTreeTask(List<Pair<V, V>> immediateDominators, V root, Class<E> edgeClass) {
        this.immediateDominators = immediateDominators;
        this.root = root;
        this.edgeClass = edgeClass;
    }

    public DominatorTree<V, E> run() {
        Graph<V, E> dominatorTree = new DefaultDirectedGraph<>(edgeClass);
        immediateDominators.forEach(pair -> dominatorTree.addVertex(pair.getLeft()));
        immediateDominators.forEach(pair -> {
            if (pair.getRight() == null) return;
            dominatorTree.addEdge(pair.getRight(), pair.getLeft());
        });
        return new DominatorTree<>(root, dominatorTree);
    }
}
