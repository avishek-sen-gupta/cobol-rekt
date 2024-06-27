package org.smojol.common.vm.exception;

public class UnresolvedVariableReferenceException extends RuntimeException {
    public UnresolvedVariableReferenceException(String referenceName) {
        super("Unresolved variable reference: " + referenceName);
    }
}
