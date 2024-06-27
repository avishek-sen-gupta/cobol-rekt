package org.smojol.common.vm.exception;

public class UnsupportedClassConditionException extends RuntimeException {
    public UnsupportedClassConditionException(String className) {
        super("Unsupported class condition: " + className);
    }
}
