package org.smojol.common.ast;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolSentenceParser;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class CommentExtraction {
    public List<CommentBlock> run(Path sourcePath, CobolEntityNavigator navigator) throws IOException {
        List<CommentBlock> allCommentBlocks = new ArrayList<>();
        CommentBlock currentBlock = new CommentBlock();
        List<String> lines = Files.readAllLines(sourcePath, StandardCharsets.ISO_8859_1)
                .stream().filter(l -> l.trim().length() > 7).toList();
        List<String> linesWithoutAreaA = lines.stream().map(l -> l.substring(6)).toList();
        for (String line : linesWithoutAreaA) {
            if (line.startsWith("*")) {
                if (!containsWords(line) || validCobol(line.substring(1))) continue;
                if (currentBlock == null) currentBlock = new CommentBlock();
                currentBlock.add(line);
            } else {
                if (currentBlock == null) continue;
                currentBlock.setCodeContext(line);
                allCommentBlocks.add(currentBlock);
                currentBlock = null;
            }
        }

        allCommentBlocks.forEach(block -> {
            ParseTree matchingNode = navigator.findNarrowestByCondition(n -> NodeText.originalText(n, NodeText::PASSTHROUGH).contains(block.getCodeContextLine().trim()));
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
