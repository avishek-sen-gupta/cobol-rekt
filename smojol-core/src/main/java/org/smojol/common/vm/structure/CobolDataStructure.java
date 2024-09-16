package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;
import hu.webarticum.treeprinter.SimpleTreeNode;
import hu.webarticum.treeprinter.TreeNode;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolDataTypes;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.CommentBlock;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.structure.DataStructureContext;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;

import static org.smojol.common.vm.memory.DataLayoutBuilder.parseSpec;
import static org.smojol.common.vm.type.CobolDataType.*;

public abstract class CobolDataStructure extends SimpleTreeNode {
    @Getter protected final CobolDataType dataType;
    protected final String name;
    @Getter protected final int levelNumber;
    @Getter private final String id;
    @Getter private final String rawText;
    @Getter protected final SourceSection sourceSection;
    protected List<CobolDataStructure> structures;
    @Getter protected List<CommentBlock> commentBlocks = new ArrayList<>();
    protected CobolDataStructure parent;
    protected boolean isComposite;

    public abstract boolean isRedefinition();
    public abstract String name();
    public abstract Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme();
    public abstract String content();
    public abstract MemoryLayout layout();
    public abstract List<CobolDataStructure> matches(String recordID);
    public abstract CobolDataStructure addConditionalVariable(ConditionalDataStructure conditionalDataStructure);
    public abstract CobolDataStructure copy(Function<CobolParser.DataDescriptionEntryFormat1Context, String> namingScheme);
    public abstract void set(CobolReference ref);
    public abstract void reset();
    public abstract TypedRecord getValue();
    public abstract CobolDataStructure cobolIndex(int index);
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

    public List<CobolDataStructure> subStructures() {
        return new ArrayList<>(structures);
    }

    protected CobolDataStructure(String name, int levelNumber, CobolDataType dataType, String rawText, SourceSection sourceSection) {
        this(name, new ArrayList<>(), levelNumber, null, false, dataType, rawText, sourceSection);
    }

    // Root constructor
    private CobolDataStructure(int levelNumber) {
        this("[ROOT]", levelNumber, CobolDataType.ROOT, "[ROOT]", SourceSection.ROOT);
    }

    // Copy constructor
    protected CobolDataStructure(String name, List<CobolDataStructure> childStructures, int level, CobolDataStructure parent, boolean isComposite, CobolDataType dataType, String rawText, SourceSection sourceSection) {
        super(name);
        this.rawText = rawText;
        this.sourceSection = sourceSection;
        // TODO: Inject ID Provider. ID Provider is already present in CobolDataStructureBuilder, inject it into all the constructors
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

    public CobolDataStructure addChildren(List<? extends CobolDataStructure> dataStructures) {
        dataStructures.forEach(this::addChild);
        return this;
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
        return searchRecursively(subRecord.getText(), this);
    }

    public TypedRecord value(String subRecordID) {
        return reference(subRecordID).getValue();
    }

    public CobolDataStructure reference(String subRecordID) {
        List<? extends CobolDataStructure> path = searchRecursively(subRecordID, this);
        if (path.isEmpty()) return new NullDataStructure(subRecordID);
        return path.getLast();
    }

    public List<? extends CobolDataStructure> searchRecursively(String subRecordID, CobolDataStructure currentStructure) {
        List<CobolDataStructure> matches = currentStructure.matches(subRecordID);
        if (!matches.isEmpty())
            return matches;
        for (CobolDataStructure structure : currentStructure.structures) {
            List<? extends CobolDataStructure> results = searchRecursively(subRecordID, structure);
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
        if (dataDescription.dataOccursClause() != null && !dataDescription.dataOccursClause().isEmpty())
            return CobolDataType.TABLE;
        else if (isPointer(dataDescription)) return CobolDataType.POINTER;
        if (dataDescription.dataPictureClause().isEmpty()) return GROUP;

        // TODO: Handle multiple usage clauses?
        String input = dataDescription.dataPictureClause().getFirst().pictureString().getFirst().getText();
        CobolDataTypes.StartRuleContext root = parseSpec(input);
        if (root.dataTypeSpec().fraction() != null) {
            if (dataDescription.dataUsageClause().isEmpty()) return NUMERIC_EXTERNAL_DECIMAL;

            CobolParser.UsageFormatContext usageFormatContext = dataDescription.dataUsageClause().getFirst().usageFormat();
            if (usageFormatContext.COMP_3() != null
                    || usageFormatContext.COMPUTATIONAL_3() != null
                    || usageFormatContext.PACKED_DECIMAL() != null)
                return COMPUTATIONAL3_DECIMAL;
            return NUMERIC_EXTERNAL_DECIMAL;
        } else if (root.dataTypeSpec().alphanumeric() != null) return STRING;
        throw new UnsupportedOperationException("Unknown type: " + root.dataTypeSpec().getText());
    }

    private static <T> boolean chain(CobolParser.DataDescriptionEntryFormat1Context dataDescription, Supplier<T> levelNumber) {
        return false;
    }

    private static boolean isPointer(CobolParser.DataDescriptionEntryFormat1Context dataDescription) {
        return !dataDescription.dataUsageClause().isEmpty()
                && (dataDescription.dataUsageClause().getFirst().usageFormat().POINTER() != null
                || dataDescription.dataUsageClause().getFirst().usageFormat().POINTER_32() != null
                || dataDescription.dataUsageClause().getFirst().usageFormat().PROCEDURE_POINTER() != null
                || dataDescription.dataUsageClause().getFirst().usageFormat().FUNCTION_POINTER() != null
        );
    }

    protected TypedRecord typed(Object v) {
        if (dataType == STRING || dataType == GROUP) return TypedRecord.typedString(v.toString());
        if (dataType == NUMERIC_EXTERNAL_DECIMAL || dataType == COMPUTATIONAL3_DECIMAL)
            return TypedRecord.typedNumber(Double.parseDouble(v.toString()));
        if (dataType == BOOLEAN) return TypedRecord.typedBoolean(Boolean.parseBoolean(v.toString()));
        if (dataType == ROOT) return TypedRecord.typedBoolean(Boolean.parseBoolean(v.toString()));
        throw new IllegalArgumentException("Can't get value of type " + dataType + " of structure " + v.toString());
    }

    public CobolDataStructure index(int index) {
        return structures.get(index);
    }

    public void accept(DataStructureVisitor visitor, CobolDataStructure parent, Function<CobolDataStructure, Boolean> stopRecurseCondition, CobolDataStructure root) {
        CobolDataStructure parentNode = visitor.visit(this, parent, root);
        if (stopRecurseCondition.apply(this)) return;
        this.structures.forEach(s -> s.accept(visitor, parentNode, stopRecurseCondition, root));
    }

    public void acceptScopedVisitor(ScopedDataStructureVisitor visitor) {
        ScopedDataStructureVisitor scopedVisitor = visitor.visit(this);
        this.structures.forEach(s -> s.acceptScopedVisitor(scopedVisitor));
    }

    public void addComment(CommentBlock cb) {
        commentBlocks.add(cb);
    }

    public DataStructureContext dataCategory() {
        return switch (sourceSection) {
            case LINKAGE -> DataStructureContext.PARAMETER;
            case WORKING_STORAGE -> DataStructureContext.GLOBAL;
            case ROOT -> DataStructureContext.ROOT;
            case NONE -> DataStructureContext.NONE;
            case PROCEDURE_DIVISION -> DataStructureContext.LOCAL;
            case FILE_DESCRIPTOR -> DataStructureContext.IO;
        };
    }
}
