package org.smojol.toolkit.analysis.pipeline;

import io.vavr.Function1;
import io.vavr.Function2;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.resource.ResourceOperations;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import static org.apache.commons.lang3.tuple.ImmutablePair.nullPair;

public class ProgramSearch {
    private static final Logger LOGGER = Logger.getLogger(ProgramSearch.class.getName());
    public static final ImmutablePair<File, String> NO_PATH = ImmutablePair.nullPair();
    public static final Function2<String, File, Boolean> STRICT = (name, f) -> name.equals(f.getName());
    public static final Function2<String, File, Boolean> PERMISSIVE = (name, f) -> name.equalsIgnoreCase(f.getName()) ||
            String.format("%s.cbl", name.toUpperCase()).equals(f.getName().toUpperCase()) ||
            f.getName().toUpperCase().contains(name.toUpperCase());
    private final Function2<String, File, Boolean> matchStrategy;
    private final ResourceOperations resourceOperations;

    public ProgramSearch() {
        this(STRICT);
    }

    public ProgramSearch(Function2<String, File, Boolean> matchStrategy) {
        this(matchStrategy, new LocalFilesystemOperations());
    }

    public ProgramSearch(Function2<String, File, Boolean> matchStrategy, ResourceOperations resourceOperations) {
        this.matchStrategy = matchStrategy;
        this.resourceOperations = resourceOperations;
    }

    public ProgramSearch(ResourceOperations resourceOperations) {
        this(STRICT, resourceOperations);
    }

    public static ProgramSearch searchStrategy(boolean isPermissiveSearch) {
        return isPermissiveSearch ? new ProgramSearch(ProgramSearch.PERMISSIVE) : new ProgramSearch();
    }

    public Pair<File, String> run(String program, String sourceDir) {
        LOGGER.info("Searching for program: " + program + "...");
//        Collection<File> files = FileUtils.listFiles(new File(sourceDir), null, true);
        Collection<File> files = resourceOperations.listFiles(new File(sourceDir));
        Function1<File, Boolean> curriedMatch = matchStrategy.curried().apply(program);
        List<File> matchingFiles = files.stream().filter(curriedMatch::apply).toList();
        if (matchingFiles.isEmpty()) {
            LOGGER.info(ConsoleColors.red("No files matching " + program + " in " + sourceDir + ". Terminating recursion..."));
            ImmutablePair<File, String> empty = nullPair();
            return empty;
        }
        File foundFile = matchingFiles.getFirst();
        String srcDir = foundFile.getParent();
        LOGGER.info("Found " + foundFile.getName() + " in " + srcDir);

        return ImmutablePair.of(foundFile, srcDir);
    }
}
