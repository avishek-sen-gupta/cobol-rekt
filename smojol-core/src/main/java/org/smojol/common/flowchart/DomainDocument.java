package org.smojol.common.flowchart;

import lombok.Getter;
import lombok.Setter;

public class DomainDocument {
    @Getter
    @Setter
    private String text;

    public DomainDocument(String text) {
        this.text = text;
    }

    public DomainDocument() {
        this("");
    }

    public boolean isEmpty() {
        return text.isEmpty();
    }
}
