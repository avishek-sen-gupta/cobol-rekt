package org.smojol.toolkit.analysis.task.transpiler;

import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.transpiler.AnnotatedEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;

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
