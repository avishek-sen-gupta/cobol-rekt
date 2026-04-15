package org.smojol.common.ast;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolSentenceParser;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class CommentExtraction {
    public List<CommentBlock> run(Path sourcePath, CobolEntityNavigator navigator) throws IOException {
        List<CommentBlock> allCommentBlocks = new ArrayList<>();
        CommentBlock currentBlock = new CommentBlock();
        String[] lineArray = new String(Files.readAllBytes(sourcePath)).split("\n");
        for (int i = 0; i < lineArray.length; i++) {
            String rawLine = lineArray[i];
            if (rawLine.length() <= 7) continue;
            String line = rawLine.substring(6);
            if (line.startsWith("*")) {
                if (!containsWords(line)) continue;
                if (currentBlock == null) currentBlock = new CommentBlock();
                currentBlock.add(line);
            } else {
                if (currentBlock == null) continue;
                currentBlock.setCodeContext(line, i + 1); // 1-based line number to match ANTLR
                allCommentBlocks.add(currentBlock);
                currentBlock = null;
            }
        }

        allCommentBlocks.forEach(block -> {
            ParseTree matchingNode;
            if (block.getCodeContextLineNumber() > 0) {
                // Match by line number to avoid ambiguity when identical statements exist
                matchingNode = navigator.findNarrowestByCondition(n -> {
                    if (n instanceof ParserRuleContext ctx && ctx.getStart() != null) {
                        int startLine = ctx.getStart().getLine();
                        int stopLine = ctx.getStop() != null ? ctx.getStop().getLine() : startLine;
                        return block.getCodeContextLineNumber() >= startLine && block.getCodeContextLineNumber() <= stopLine;
                    }
                    return false;
                });
            } else {
                // Fallback to text-based matching for backward compatibility
                matchingNode = navigator.findNarrowestByCondition(n -> NodeText.originalText(n, NodeText::PASSTHROUGH).contains(block.getCodeContextLine().trim()));
            }
            block.setAssociatedTree(matchingNode);
        });
        return allCommentBlocks;
    }

    private static boolean containsWords(String line) {
        return line.chars().mapToObj(c -> Character.isAlphabetic(c) || Character.isDigit(c)).reduce(false, (b, c) -> b || c);
    }

    private static boolean validCobol(String line) {
        CobolLexer antlrLexer = new CobolLexer(CharStreams.fromString(line));
        antlrLexer.removeErrorListeners();
        CommonTokenStream tokens = new CommonTokenStream(antlrLexer);
        CobolSentenceParser antlrParser = new CobolSentenceParser(tokens);
        antlrParser.removeErrorListeners();
        SentenceParserErrorListener listener = new SentenceParserErrorListener();
        antlrParser.addErrorListener(listener);
        CobolSentenceParser.StartRuleContext startRuleContext = antlrParser.startRule();
        return !listener.hasErrors();
    }
}
