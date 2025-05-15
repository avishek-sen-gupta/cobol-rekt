package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerLoop;
import com.mojo.algorithms.domain.TranspilerNode;

import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class TranspilerTreeFormatter {
    private final String indentString;

    public TranspilerTreeFormatter(String indentString) {
        this.indentString = indentString;
    }

    public TranspilerTreeFormatter() {
        this("  ");
    }

    public String format(TranspilerNode node) {
        return String.join("\n", format(node, 0));
    }

    public List<String> format(TranspilerNode n, int level) {
        return switch (n) {
            case LabelledTranspilerCodeBlockNode l -> blockFormat(l, level);
            case TranspilerCodeBlockNode l -> blockFormat(l, level);
            case IfTranspilerNode l -> ifFormat(l, level);
            case TranspilerLoop l -> loopFormat(l, level);
            default -> ImmutableList.of(indented(n.description(), level));
        };
    }

    private String indented(String code, int level) {
        return IntStream.range(0, level).mapToObj(i -> indentString).reduce("", String::concat) + code;
    }

    private List<String> loopFormat(TranspilerLoop l, int level) {
        return Stream.concat(Stream.concat(
                        Stream.of(indented(String.format("Loop [%s] {", l.headerDescription()), level)),
                        l.astChildren().stream().flatMap(c -> format(c, level + 1).stream())
                ),
                Stream.of(indented("}", level))).toList();
    }

    private List<String> ifFormat(IfTranspilerNode l, int level) {
        return Stream.of(
                Stream.of(indented(String.format("if (%s)", l.getCondition().description()), level)),
                format(l.getIfThenBlock(), level).stream(),
                Stream.of(indented("else", level)),
                format(l.getIfElseBlock(), level).stream()
        ).flatMap(s -> s).toList();
    }

    private List<String> blockFormat(TranspilerCodeBlockNode l, int level) {
        return Stream.of(
                Stream.of(indented("{", level)),
                l.astChildren().stream().flatMap(c -> format(c, level + 1).stream()),
                Stream.of(indented("}", level))).flatMap(s -> s).toList();
    }

    private List<String> blockFormat(LabelledTranspilerCodeBlockNode l, int level) {
        return Stream.concat(Stream.concat(
                        Stream.of(indented(String.format("BLOCK [%s] {", l.getName()), level)),
                        l.astChildren().stream().flatMap(c -> format(c, level + 1).stream())
                ),
                Stream.of(indented("}", level))).toList();
    }
}
