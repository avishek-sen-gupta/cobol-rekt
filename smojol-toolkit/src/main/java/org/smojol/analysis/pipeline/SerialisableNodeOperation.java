package org.smojol.analysis.pipeline;

import com.google.common.collect.ImmutableMap;
import org.jgrapht.alg.similarity.ZhangShashaTreeEditDistance;
import org.smojol.analysis.graph.graphml.TypedGraphVertex;

import java.util.Map;

public class SerialisableNodeOperation {

    private final ZhangShashaTreeEditDistance.OperationType type;
    private final Map<String, String> firstOperand;
    private final Map<String, String> secondOperand;

    public SerialisableNodeOperation(ZhangShashaTreeEditDistance.EditOperation<TypedGraphVertex> op) {
        type = op.getType();
        firstOperand = ImmutableMap.of("name", op.getFirstOperand().name(), "type", op.getFirstOperand().type(), "text", op.getFirstOperand().text());
        secondOperand = op.getSecondOperand() == null ? ImmutableMap.of() :
                ImmutableMap.of("name", op.getSecondOperand().name(), "type", op.getSecondOperand().type(), "text", op.getSecondOperand().text());
    }
}
