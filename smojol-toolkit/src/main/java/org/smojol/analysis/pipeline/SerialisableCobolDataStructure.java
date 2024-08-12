package org.smojol.analysis.pipeline;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;

import java.util.ArrayList;
import java.util.List;

public class SerialisableCobolDataStructure {
    private String id;
    private final String name;
    private String content;
    private int levelNumber;
    private String rawText;
    private final List<SerialisableCobolDataStructure> children = new ArrayList<>();
    private boolean isRedefinition;
    private String redefines;
    private final String nodeType = "DATA_VERTEX";

    public SerialisableCobolDataStructure(CobolDataStructure data) {
        name = data.name();
        content = data.content();
        id = data.getId();
        levelNumber = data.getLevelNumber();
        rawText = data.getRawText();
        isRedefinition = data.getClass() == Format1DataStructure.class && data.isRedefinition();
        redefines = isRedefinition ? ((Format1DataStructure) data).getDataDescription().dataRedefinesClause().getFirst().dataName().getText() : "";
    }

    public SerialisableCobolDataStructure() {
        this.name = "ROOT";
    }

    public void add(SerialisableCobolDataStructure child) {
        children.add(child);
    }
}
