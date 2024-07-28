package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;
import hu.webarticum.treeprinter.SimpleTreeNode;
import hu.webarticum.treeprinter.TreeNode;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.memory.DataLayoutBuilder;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;

public abstract class CobolDataStructure extends SimpleTreeNode {
    @Getter protected final CobolDataType dataType;
    private final String name;
    @Getter private final int levelNumber;
    @Getter private final String id;
    protected List<CobolDataStructure> structures;
    protected CobolDataStructure parent;
    protected boolean isComposite;

    protected abstract boolean isRedefinition();
    public abstract String name();
    public abstract Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme();
    public abstract String content();
    public abstract MemoryLayout layout();
    public abstract List<CobolDataStructure> matches(String recordID);

    public abstract CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure);
    public abstract CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme);
    public abstract void set(CobolReference ref);
    @Deprecated public abstract void set(String destinationRecordID, CobolReference ref);
    @Deprecated public abstract void reset(String recordID);
    public abstract void reset();
    public abstract TypedRecord getValue();
    public abstract CobolDataStructure cobolIndex(int index);

    @Deprecated public abstract void add(String recordID, CobolReference ref);
    @Deprecated public abstract void subtract(String recordID, CobolReference ref);
    @Deprecated public abstract void multiply(String recordID, CobolReference ref);
    @Deprecated public abstract void divide(String recordID, CobolReference ref);

    public abstract void add(CobolReference ref);
    public abstract void subtract(CobolReference ref);
    public abstract void multiply(CobolReference ref);
    public abstract void divide(CobolReference ref);

    public abstract int allocateLayouts(int headPointer, MemoryRegion region);
    public abstract void expandTables();
    public abstract void calculateMemoryRequirements();
    public abstract void allocateRecordPointers();
    public abstract boolean buildRedefinitions(CobolDataStructure root);
    public abstract int size();
    protected abstract void internalSet(TypedRecord r);

    @Override
    public List<TreeNode> children() {
        return new ArrayList<>(structures);
    }

    public CobolDataStructure(String name, int levelNumber, CobolDataType dataType) {
        this(name, new ArrayList<>(), levelNumber, null, false, dataType);
    }

    // Root constructor
    public CobolDataStructure(int levelNumber) {
        this("[ROOT]", levelNumber, CobolDataType.ROOT);
    }

    // Copy constructor
    protected CobolDataStructure(String name, List<CobolDataStructure> childStructures, int level, CobolDataStructure parent, boolean isComposite, CobolDataType dataType) {
        super(name);
        // TODO: Inject ID Provider. ID Provider is already present in DataStructureBuilder, inject it into all the constructors
        this.id = UUID.randomUUID().toString();
        this.name = name;
        this.dataType = dataType;
        this.levelNumber = level;
        this.structures = childStructures;
        this.parent = parent;
        this.isComposite = isComposite;
        structures.forEach(s -> s.setParent(this));
    }

    public CobolDataStructure parent() {
        return parent;
    }

    public CobolDataStructure addChild(CobolDataStructure dataStructure) {
        this.isComposite = true;
        dataStructure.setParent(this);
        structures.add(dataStructure);
        super.addChild(dataStructure);
        return dataStructure;
    }

    public void setParent(CobolDataStructure dataStructure) {
        this.parent = dataStructure;
    }

    public int level() {
        return levelNumber;
    }

    public CobolDataStructure parent(int level) {
        CobolDataStructure current = this;

        // If we find a node at the required entry level, it's not enough
        // We need to find a node of an entry level smaller than this to get the parent candidate
        while (current != null && current.level() >= level) {
            current = current.parent();
        }
        if (current == null) throw new RuntimeException("Parent of level " + level + " does not exist!");
        return current;
    }

    public CobolDataStructure addPeer(CobolDataStructure peer) {
        return parent.addChild(peer);
    }

    public void report() {
        new ListingTreePrinter().print(this);
    }

    public List<? extends CobolDataStructure> rootRecord(CobolParser.GeneralIdentifierContext subRecord) {
        return searchRecursively(subRecord.getText(), this, new AccessChain(ImmutableList.of(new StaticAccessLink(this))));
    }

    public TypedRecord value(String subRecordID) {
        return reference(subRecordID).getValue();
    }

    public CobolDataStructure reference(String subRecordID) {
        List<? extends CobolDataStructure> path = searchRecursively(subRecordID, this, new AccessChain(ImmutableList.of(new StaticAccessLink(this))));
        if (path.isEmpty()) return new NullDataStructure(subRecordID);
        return path.getLast();
    }

    public List<? extends CobolDataStructure> searchRecursively(String subRecordID, CobolDataStructure currentStructure, AccessChain chain) {
        List<CobolDataStructure> matches = currentStructure.matches(subRecordID);
        if (!matches.isEmpty())
            return matches;
        for (CobolDataStructure structure : currentStructure.structures) {
            List<? extends CobolDataStructure> results = searchRecursively(subRecordID, structure, new AccessChain(ImmutableList.of(new StaticAccessLink(this))));
            if (results.isEmpty()) continue;
            List<CobolDataStructure> path = new ArrayList<>(ImmutableList.of(currentStructure));
            path.addAll(results);
            return path;
        }

        return ImmutableList.of();
    }

    public AccessChain chain(String subRecordID) {
        AccessChain path = chain(subRecordID, this, new AccessChain(ImmutableList.of(new StaticAccessLink(this))));
        return path;
    }

    public AccessChain chain(String subRecordID, CobolDataStructure currentStructure, AccessChain chain) {
        List<CobolDataStructure> matches = currentStructure.matches(subRecordID);
        if (!matches.isEmpty()) {
            if (currentStructure.getDataType() == CobolDataType.TABLE) return chain.curriedIndex();
            return chain;
        }
        return currentStructure.typeSpecificChain(subRecordID, chain);
    }

    protected abstract AccessChain typeSpecificChain(String subRecordID, AccessChain chain);

    protected static CobolDataType cobolDataType(CobolParser.DataDescriptionEntryFormat1Context dataDescription) {
        return new DataLayoutBuilder().type(dataDescription);
    }

    protected TypedRecord typed(Object v) {
        return switch (dataType) {
            case STRING, GROUP -> TypedRecord.typedString(v.toString());
            case NUMBER -> TypedRecord.typedNumber(Double.parseDouble(v.toString()));
            case BOOLEAN -> TypedRecord.typedBoolean(Boolean.parseBoolean(v.toString()));
            case ROOT -> throw new IllegalArgumentException("Can't get value of root structure");
            default ->
                    throw new IllegalArgumentException("Can't get value of type " + dataType + " of structure " + v.toString());
        };
    }

    public CobolDataStructure index(int index) {
        return structures.get(index);
    }

    public void accept(DataStructureVisitor visitor, CobolDataStructure parent, Function<CobolDataStructure, Boolean> stopRecurseCondition, CobolDataStructure root) {
        CobolDataStructure parentNode = visitor.visit(this, parent, root);
        if (stopRecurseCondition.apply(this)) return;
        this.structures.forEach(s -> s.accept(visitor, parentNode, stopRecurseCondition, root));
    }
}
