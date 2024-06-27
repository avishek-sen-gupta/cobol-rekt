package org.smojol.common.vm.exception;

public class IllegalIndexException extends RuntimeException {
    public IllegalIndexException(int index, String cause) {
        super("Illegal indexing attempt: " + index + ". Cause: " + cause);
    }
}
