package org.smojol.toolkit.analysis.graph;

// TODO: Move to common
public class NamespaceQualifier {
    private final String namespace;
    public NamespaceQualifier(String namespace) {
        this.namespace = namespace;
    }
    public String getNamespace() {
        return namespace;
    }
}
