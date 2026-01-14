package org.smojol.jcl.model;

/**
 * Exception thrown when JCL parsing fails.
 */
public class JclParsingException extends Exception {
    
    public JclParsingException(String message) {
        super(message);
    }
    
    public JclParsingException(String message, Throwable cause) {
        super(message, cause);
    }
}
