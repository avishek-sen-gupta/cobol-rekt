package org.smojol.toolkit.flowchart;

import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.model.MutableNode;

public class GraphvizStyleScheme {
    private final Color backgroundFillColor;
    private final Color fontColor;
    private final Shape shape;
    private final String style;
//    private final DomainDocument notes;

    public GraphvizStyleScheme(Color backgroundFillColor, Color fontColor, Shape shape) {
        this(backgroundFillColor, fontColor, shape, "filled");
    }

    public GraphvizStyleScheme(Color backgroundFillColor, Color fontColor, Shape shape, String style) {
        this.backgroundFillColor = backgroundFillColor;
        this.fontColor = fontColor;
        this.shape = shape;
        this.style = style;
    }
//    public GraphvizStyleScheme(Color backgroundFillColor, Color fontColor, Shape shape, DomainDocument notes) {
//        this.background = backgroundFillColor;
//        this.fontColor = fontColor;
//        this.shape = shape;
//        this.notes = notes;
//    }

    public GraphvizStyleScheme(Color backgroundFillColor, Color fontColor) {
        this(backgroundFillColor, fontColor, Shape.BOX);
    }

    public MutableNode apply(MutableNode node) {
        MutableNode styledNode = node.add("style", style).add("fontcolor", fontColor.value).add("fillcolor", backgroundFillColor.value).add("shape", shape.value);
        return styledNode;
//        return notes.isEmpty() ? styledNode : styledNode.add("label", notes.getText());
    }
}
