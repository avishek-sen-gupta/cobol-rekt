package org.smojol.common.ast;

import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.ArrayList;
import java.util.List;

public class CommentBlock {
    private final List<String> lines = new ArrayList<>();
    @Getter private String codeContextLine;
    private ParseTree nodeContext;

    public void add(String commentLine) {
        lines.add(commentLine);
    }

    public void setCodeContext(String codeContextLine) {
        this.codeContextLine = codeContextLine;
    }

    public void setAssociatedTree(ParseTree nodeContext) {
        this.nodeContext = nodeContext;
    }
}
