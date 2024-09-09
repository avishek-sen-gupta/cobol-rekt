package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public abstract class CobolExpression {
    @Getter protected final List<CobolExpression> children = new ArrayList<>();
    @Getter private final String id;
    protected final String operationMnemonic;

    public CobolExpression(String operationMnemonic) {
        this(ImmutableList.of(), operationMnemonic);
    }

    public CobolExpression(List<CobolExpression> children, String operationMnemonic) {
        this.operationMnemonic = operationMnemonic;
        this.id = UUID.randomUUID().toString();
        this.children.addAll(children);
    }

    public abstract CobolExpression evaluate(CobolDataStructure data);
    public abstract String description();
    public abstract AbstractCobolType expressionType(CobolDataStructure dataStructures);

    public CobolDataStructure reference(CobolDataStructure data) {
        throw new UnsupportedOperationException("Cannot resolve to references of intermediate expressions");
    }

    public double evalAsNumber(CobolDataStructure data) {
        return evaluate(data).evalAsNumber(data);
    }

    public boolean evalAsBoolean(CobolDataStructure data) {
        return evaluate(data).evalAsBoolean(data);
    }
    public String evalAsString(CobolDataStructure data) {
        return evaluate(data).evalAsString(data);
    }

    public CobolExpression equalTo(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).equalTo(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression lessThan(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).lessThan(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression greaterThan(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).greaterThan(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression lessThanOrEqualTo(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).lessThanOrEqualTo(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression greaterThanOrEqualTo(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).greaterThanOrEqualTo(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression add(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).add(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression and(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).and(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression or(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).or(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression subtract(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).subtract(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression divide(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).divide(other.evaluate(dataStructures), dataStructures);
    }

    public CobolExpression multiply(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).multiply(other.evaluate(dataStructures), dataStructures);
    }

    protected CobolExpression exponent(CobolExpression other, CobolDataStructure dataStructures) {
        return evaluate(dataStructures).exponent(other.evaluate(dataStructures), dataStructures);
    }

    protected CobolExpression negative(CobolDataStructure dataStructures) {
        return evaluate(dataStructures).negative(dataStructures);
    }

    protected CobolExpression not(CobolDataStructure dataStructures) {
        return evaluate(dataStructures).not(dataStructures);
    }

    public void accept(CobolExpressionVisitor visitor) {
        CobolExpressionVisitor scopedVisitor = visitor.visit(this);
        children.forEach(child -> child.accept(scopedVisitor));
    }

    public void acceptDepthFirst(CobolExpressionVisitor visitor) {
        CobolExpressionVisitor scopedVisitor = visitor.visit(this);
        children.forEach(child -> child.acceptDepthFirst(scopedVisitor));
    }

    @Override
    public String toString() {
        return description();
    }
}
