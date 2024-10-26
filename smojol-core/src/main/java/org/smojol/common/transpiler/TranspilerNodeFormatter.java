package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;

import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class TranspilerNodeFormatter {
    private final String indentString;

    public TranspilerNodeFormatter(String indentString) {
        this.indentString = indentString;
    }

    public TranspilerNodeFormatter() {
        this("  ");
    }

    private String tabbed(String code, int level) {
        return IntStream.range(0, level).mapToObj(i -> indentString).reduce("", String::concat) + code;
    }

    public List<String> format(TranspilerNode n, int level) {
        return switch (n) {
            case LabelledTranspilerCodeBlockNode l -> blockFormat(l, level);
            case TranspilerCodeBlockNode l -> blockFormat(l, level);
            case IfTranspilerNode l -> ifFormat(l, level);
            case TranspilerLoop l -> loopFormat(l, level);
            default -> ImmutableList.of(tabbed(n.description(), level));
        };
    }

    private List<String> loopFormat(TranspilerLoop l, int level) {
        return Stream.concat(Stream.concat(
                        Stream.of(tabbed(String.format("Loop [%s] {", l.headerDescription()), level)),
                        l.astChildren().stream().flatMap(c -> format(c, level + 1).stream())
                ),
                Stream.of(tabbed("}", level))).toList();
    }

    private List<String> ifFormat(IfTranspilerNode l, int level) {
        return Stream.of(
                Stream.of(tabbed(String.format("if (%s)", l.getCondition().description()), level)),
                format(l.getIfThenBlock(), level).stream(),
                Stream.of(tabbed("else", level)),
                format(l.getIfElseBlock(), level).stream()
        ).flatMap(s -> s).toList();
    }

    private List<String> blockFormat(TranspilerCodeBlockNode l, int level) {
        return Stream.of(
                Stream.of(tabbed("{", level)),
                l.astChildren().stream().flatMap(c -> format(c, level + 1).stream()),
                Stream.of(tabbed("}", level))).flatMap(s -> s).toList();
    }

    private List<String> blockFormat(LabelledTranspilerCodeBlockNode l, int level) {
        return Stream.concat(Stream.concat(
                        Stream.of(tabbed(String.format("BLOCK [%s] {", l.getName()), level)),
                        l.astChildren().stream().flatMap(c -> format(c, level + 1).stream())
                ),
                Stream.of(tabbed("}", level))).toList();
    }

    public String format(TranspilerNode node) {
        return String.join("\n", format(node, 0));
    }
}
