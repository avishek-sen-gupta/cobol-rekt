package org.smojol.common.ast;

public interface FlowNodeCondition {
    FlowNodeCondition ALWAYS_SHOW = new FlowNodeCondition() {
        @Override
        public boolean apply(FlowNode node) {
            return false;
        }
    };

    boolean apply(FlowNode node);
}
