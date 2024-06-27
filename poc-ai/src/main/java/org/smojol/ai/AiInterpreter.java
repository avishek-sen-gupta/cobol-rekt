package org.smojol.ai;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.analysis.ParsePipeline;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.flowchart.GraphGenerator;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AiInterpreter {
    private final CobolEntityNavigator navigator;
    private final ParsePipeline pipeline;
    private HashMap<String, SymbolReferences> responseToReferences = new HashMap<>();
    private List<SymbolReferences> references;
    private FlowchartBuilder flowcharter;
    private final ParseTree scope;

    public AiInterpreter(ParsePipeline pipeline, ParseTree scope) {
        this.pipeline = pipeline;
        this.navigator = pipeline.getNavigator();
        this.flowcharter = pipeline.flowcharter();
        this.scope = scope;
    }

    public SymbolReferences references(String responseLine) {
        SymbolReferences allSymbols = new SymbolReferences(navigator, scope);
        Pattern pattern = Pattern.compile("\\b([A-Z0-9\\-]+)\\b");
        Matcher matcher = pattern.matcher(responseLine);
        while (matcher.find()) {
            allSymbols.add(matcher.group());
        }

        responseToReferences.put(responseLine, allSymbols);
        return allSymbols;
    }

    public void extractReferences(List<String> responses) {
        List<String> lines = asSeparateLines(responses);
        references = lines.stream().map(this::references).toList();
        references.forEach(r -> {
            System.out.println(r);
        });
    }

    private static List<String> asSeparateLines(List<String> responses) {
        return Arrays.asList(responses.get(0).split("\n")).stream().filter(s -> !s.isEmpty()).toList();
    }

    public void buildFlowchart() {
        flowcharter.buildChartAST(scope);
        flowcharter.buildOverlay();
        flowcharter.buildDotStructure();
//        references.forEach(r -> {
//            r.getSymbols().forEach(s -> flowcharter.draw(s));
//        });
    }


    public void write(String dotFilePath, String graphOutputPath) throws IOException, InterruptedException {
        for (Map.Entry<String, SymbolReferences> responseSymbolSet : responseToReferences.entrySet()) {
            SymbolReferences references = responseSymbolSet.getValue();
            flowcharter.createComment(responseSymbolSet.getKey());
            for (List<ParseTree> symbolReferences: references.getSymbols()) {
                symbolReferences.forEach(ref -> flowcharter.connectToComment(responseSymbolSet.getKey(), ref));
            }
        }
        flowcharter.write(dotFilePath);
        new GraphGenerator("ortho").generateImage(dotFilePath, graphOutputPath);
    }
}
