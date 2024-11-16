package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

import java.util.List;
import java.util.Map;

@Getter
public class LabelledTranspilerCodeBlockNode extends TranspilerCodeBlockNode {
    private final String name;

    public LabelledTranspilerCodeBlockNode(String name, List<TranspilerNode> children, Map<String, Object> properties) {
        super(children, properties, ImmutableList.of(SemanticCategory.CODE_BLOCK));
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("[CODE_BLOCK: %s] : ", name) + " {\n" + String.join("\n", childTranspilerNodes.stream().map(TranspilerNode::description).toList()) + "\n}\n";
//        return String.format("[%s]", name);
    }

    @Override
    public String shortDescription() {
        return String.format("[CODE_BLOCK: %s] : ", name);
    }
}
