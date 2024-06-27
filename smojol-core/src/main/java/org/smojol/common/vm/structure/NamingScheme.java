package org.smojol.common.vm.structure;

import org.eclipse.lsp.cobol.core.CobolParser;

import java.util.function.Function;

public class NamingScheme {
    public static Function<CobolParser.DataDescriptionEntryFormat1Context, String> IDENTITY = d -> d.entryName().getText();
    public static Function<CobolParser.DataDescriptionEntryFormat1Context, String> ROOT = d -> "[ROOT]";
    public static Function<CobolParser.DataDescriptionEntryFormat1Context, String> NULL = d -> "[NULL]";
    public static Function<Integer, Function<CobolParser.DataDescriptionEntryFormat1Context, String>> INDEXED = index -> ctx -> ctx.entryName().getText() + "$" + index;
}
