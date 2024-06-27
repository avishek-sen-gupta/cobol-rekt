package org.smojol.common.vm.exception;

public class UnsupportedLiteralTypeException extends RuntimeException {
    public UnsupportedLiteralTypeException(String literalValue) {
        super("This literal is not supported yet: " + literalValue);
    }
}
