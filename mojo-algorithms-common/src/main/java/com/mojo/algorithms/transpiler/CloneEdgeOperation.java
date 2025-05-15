package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.AnnotatedEdge;
import com.mojo.algorithms.domain.BackJoinEdge;
import com.mojo.algorithms.domain.CrossJoinEdge;
import com.mojo.algorithms.domain.DominatorEdge;
import com.mojo.algorithms.domain.TypedGraphEdge;
import org.jgrapht.graph.DefaultEdge;

public class CloneEdgeOperation {
    public static DefaultEdge cloneEdge(DefaultEdge e) {
        return switch (e) {
            case DominatorEdge de -> new DominatorEdge();
            case BackJoinEdge be -> new BackJoinEdge();
            case CrossJoinEdge ce -> new CrossJoinEdge();
            case AnnotatedEdge ae -> (AnnotatedEdge) ae.clone();
            case TypedGraphEdge te -> new TypedGraphEdge(te.getRelationshipType(), te.getNamespace());
            case DefaultEdge de -> new DefaultEdge();
        };
    }
}
