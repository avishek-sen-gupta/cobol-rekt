package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public class LabelledTranspilerCodeBlockNode extends TranspilerNode {
    @Getter private final String name;

    public LabelledTranspilerCodeBlockNode(String name, List<TranspilerNode> children) {
        this(name, children, ImmutableMap.of());
    }

    public LabelledTranspilerCodeBlockNode(String name, List<TranspilerNode> children, Map<String, Object> properties) {
        super(children, properties, ImmutableList.of(SemanticCategory.CODE_BLOCK));
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("[%s]", name);
    }

    @Override
    public Collection<TranspilerNode> astChildren() {
        return childTranspilerNodes;
    }
}
