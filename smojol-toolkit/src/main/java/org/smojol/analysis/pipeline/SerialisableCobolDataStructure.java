package org.smojol.analysis.pipeline;

import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

public class SerialisableCobolDataStructure {
    private String id;
    private final String name;
    private String content;
    private int levelNumber;
    private String rawText;
    private final List<SerialisableCobolDataStructure> children = new ArrayList<>();

    public SerialisableCobolDataStructure(CobolDataStructure data) {
        name = data.name();
        content = data.content();
        id = data.getId();
        levelNumber = data.getLevelNumber();
        rawText = data.getRawText();
    }

    public SerialisableCobolDataStructure() {
        this.name = "ROOT";
    }

    public void add(SerialisableCobolDataStructure child) {
        children.add(child);
    }
}
