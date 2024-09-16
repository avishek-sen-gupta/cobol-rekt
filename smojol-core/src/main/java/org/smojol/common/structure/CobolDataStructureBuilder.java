package org.smojol.common.structure;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.reference.DetachedDataStructure;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;
import org.smojol.common.vm.structure.*;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.logging.Logger;

public class CobolDataStructureBuilder {
    private static final Logger LOGGER = Logger.getLogger(CobolDataStructureBuilder.class.getName());
    private final CobolEntityNavigator navigator;
    private final UnresolvedReferenceStrategy unresolvedReferenceStrategy;
    private CobolDataStructure zerothStructure;
    private final Format1DataStructureBuilder format1DataStructureBuilder;
    private final IdProvider idProvider;

    public CobolDataStructureBuilder(CobolEntityNavigator navigator, UnresolvedReferenceStrategy unresolvedReferenceStrategy, Format1DataStructureBuilder format1DataStructureBuilder, IdProvider idProvider) {
        this.navigator = navigator;
        this.unresolvedReferenceStrategy = unresolvedReferenceStrategy;
        this.format1DataStructureBuilder = format1DataStructureBuilder;
        this.idProvider = idProvider;
    }

    public CobolDataStructure build() {
        zerothStructure = new Format1DataStructure(0, unresolvedReferenceStrategy);
        ParseTree dataDivision = navigator.dataDivisionBodyRoot();
        CobolParser.DataDivisionContext dataDivisionBody = (CobolParser.DataDivisionContext) dataDivision;
        extractFromWorkingStorage(dataDivisionBody);
        extractFromLinkage(dataDivisionBody);
        extractFromFileSection(dataDivisionBody);
        zerothStructure.expandTables();
        zerothStructure.calculateMemoryRequirements();
        zerothStructure.allocateRecordPointers();
        int i = 0;
        while (zerothStructure.buildRedefinitions(zerothStructure)) {
            LOGGER.info("Building redefinitions...");
        }
        addGlobalSystemStructures();
        addUnreferencedStructures();
        return zerothStructure;
    }

    private void addGlobalSystemStructures() {
        zerothStructure.addChild(new StaticDataStructure("WHEN-COMPILED", 1, CobolDataType.STRING, TypedRecord.typedString("01-01-2024")));
    }

    private void addUnreferencedStructures() {
        List<ParseTree> unreferencedVariables = new UnreferencedVariableSearch().run(navigator, zerothStructure);
        List<? extends CobolDataStructure> unreferencedStructures = unreferencedVariables.stream().map(uv -> new DetachedDataStructure(uv.getText(), TypedRecord.typedNumber(1))).toList();
        zerothStructure.addChildren(unreferencedStructures);

    }

    private void extractFromFileSection(CobolParser.DataDivisionContext dataDivisionBody) {
        Optional<CobolParser.DataDivisionSectionContext> maybeFileSection = dataDivisionBody.dataDivisionSection().stream().filter(s -> s.fileSection() != null).findFirst();
        if (maybeFileSection.isEmpty()) return;
        CobolParser.FileSectionContext fileSection = maybeFileSection.get().fileSection();
        List<CobolParser.DataDescriptionEntryContext> allDataDescriptions = fileSection.fileDescriptionEntry().stream().flatMap(fd -> fd.dataDescriptionEntry().stream()).toList();
        extractFrom(allDataDescriptions, zerothStructure, this::fileDescriptorData, SourceSection.FILE_DESCRIPTOR);
    }

    private void extractFromLinkage(CobolParser.DataDivisionContext dataDivisionBody) {
        Optional<CobolParser.DataDivisionSectionContext> maybeLinkageSection = dataDivisionBody.dataDivisionSection().stream().filter(s -> s.linkageSection() != null).findFirst();
        if (maybeLinkageSection.isEmpty()) return;
        CobolParser.LinkageSectionContext linkageSection = maybeLinkageSection.get().linkageSection();
        List<CobolParser.DataDescriptionEntryForWorkingStorageAndLinkageSectionContext> linkageSectionDataLayouts = linkageSection.dataDescriptionEntryForWorkingStorageAndLinkageSection();
        extractFrom(linkageSectionDataLayouts, zerothStructure, this::linkageData, SourceSection.LINKAGE);
    }

    private void extractFromWorkingStorage(CobolParser.DataDivisionContext dataDivisionBody) {
        Optional<CobolParser.DataDivisionSectionContext> maybeWorkingStorage = dataDivisionBody.dataDivisionSection().stream().filter(s -> s.workingStorageSection() != null).findFirst();
        if (maybeWorkingStorage.isEmpty()) return;
        CobolParser.WorkingStorageSectionContext workingStorageSection = maybeWorkingStorage.get().workingStorageSection();
        List<CobolParser.DataDescriptionEntryForWorkingStorageSectionContext> workingStorageDataLayouts = workingStorageSection.dataDescriptionEntryForWorkingStorageSection();
        extractFrom(workingStorageDataLayouts, zerothStructure, this::wsData, SourceSection.WORKING_STORAGE);
    }

    private CobolParser.DataDescriptionEntryContext wsData(CobolParser.DataDescriptionEntryForWorkingStorageSectionContext e) {
        return e.dataDescriptionEntryForWorkingStorageAndLinkageSection().dataDescriptionEntry();
    }

    private CobolParser.DataDescriptionEntryContext linkageData(CobolParser.DataDescriptionEntryForWorkingStorageAndLinkageSectionContext e) {
        return e.dataDescriptionEntry();
    }

    private CobolParser.DataDescriptionEntryContext fileDescriptorData(CobolParser.DataDescriptionEntryContext e) {
        return e;
    }

    // TODO: Refactor to state machine maybe
    // TODO: Refactor to use builder in addChild(), addPeer() to inject parent directly into constructor
    private <T> void extractFrom(List<T> dataLayouts, CobolDataStructure root, Function<T, CobolParser.DataDescriptionEntryContext> retriever, SourceSection sourceSection) {
        int currentLevel = 0;
        CobolDataStructure dataStructure = root;
        // TODO: Does not check if you are adding a structure under a level 77 structure, which is invalid
        for (T dataDescriptionEntry : dataLayouts) {
            CobolParser.DataDescriptionEntryContext dataDescription = retriever.apply(dataDescriptionEntry);
            if (dataDescription.dataDescriptionEntryFormat1() != null) {
                CobolParser.DataDescriptionEntryFormat1Context format1 = dataDescription.dataDescriptionEntryFormat1();
                int entryLevel = Integer.parseInt(format1.levelNumber().LEVEL_NUMBER().getSymbol().getText());
                if (currentLevel == 0) {
                    if (entryLevel != 1) {
                        LOGGER.warning("Top level variable is not level 01");
                        // TODO: Should we be strict or lax regarding top level variables not being level 01???
//                        throw new RuntimeException("Top Level entry must be 01");
                    }
                    dataStructure = dataStructure.addChild(format1(format1, unresolvedReferenceStrategy, sourceSection));
                } else if (entryLevel == currentLevel) {
                    dataStructure = dataStructure.addPeer(format1(format1, unresolvedReferenceStrategy, sourceSection));
                } else if (entryLevel > currentLevel) {
                    dataStructure = dataStructure.addChild(format1(format1, unresolvedReferenceStrategy, sourceSection));
                } else if (entryLevel == 77) {
                    dataStructure = root.addChild(format1(format1, unresolvedReferenceStrategy, sourceSection));
                    currentLevel = 0;
                    continue;
                } else if (entryLevel == 66) {
                    // TODO: Support RENAME's at some point
                    LOGGER.warning("Level 66 RENAMEs are not supported yet, skipping...");
                    continue;
                } else {
                    // This is for adding a structure at a lower level than the current level's parent.
                    dataStructure = dataStructure.parent(entryLevel).addChild(format1(format1, unresolvedReferenceStrategy, sourceSection));
                }
            } else if (dataDescription.dataDescriptionEntryFormat3() != null) {
                CobolParser.DataDescriptionEntryFormat3Context conditionalFormat = dataDescription.dataDescriptionEntryFormat3();
                dataStructure = dataStructure.addConditionalVariable(new ConditionalDataStructure(conditionalFormat, dataStructure, sourceSection));
            }
            currentLevel = dataStructure.level();
        }
    }

    private CobolDataStructure format1(CobolParser.DataDescriptionEntryFormat1Context format1Context, UnresolvedReferenceStrategy unresolvedReferenceStrategy, SourceSection sourceSection) {
        return format1DataStructureBuilder.build(format1Context, unresolvedReferenceStrategy, sourceSection);
    }


}
