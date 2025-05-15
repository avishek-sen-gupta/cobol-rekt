package com.mojo.algorithms.domain;

import org.apache.commons.text.StringEscapeUtils;
import org.jgrapht.Graph;
import com.mojo.algorithms.id.Identifiable;

import java.util.ArrayList;
import java.util.List;

public class MermaidGraph<V extends Identifiable, E> {
    public String draw(Graph<V, E> graph) {
        List<String> lines = new ArrayList<>();
        lines.add("---");
        lines.add("title: Graph ");
        lines.add("---");
        lines.add("```mermaid");
        lines.add("flowchart TD");
        graph.vertexSet().forEach(v -> lines.add(node(v)));
        graph.edgeSet().forEach(e -> lines.add(directed(graph.getEdgeSource(e), graph.getEdgeTarget(e))));
        lines.add("```");
        return String.join("\n", lines);
    }

    private static String escaped(String s) {
        return StringEscapeUtils.escapeHtml4(s).replace("\n", "<br>");
    }

    private String directed(V from, V to) {
        return styledEdge(from, to, "-->");
    }

    private String styledEdge(V from, V to, String edgeStyle) {
        return node(from) + " " + edgeStyle + " " + node(to);
    }

    private String node(V v) {
        return String.format("%s[\"%s\"]", v.id(), escaped(v.label()));
    }
}
