package org.smojol.common.flowchart;

public record FlowchartOutputFormat(String extension, String lineStyle) {
    public static FlowchartOutputFormat PNG = new FlowchartOutputFormat("png", "ortho");
    public static FlowchartOutputFormat SVG = new FlowchartOutputFormat("svg", "ortho");
    public static FlowchartOutputFormat MERMAID = new FlowchartOutputFormat("md", "");
}

