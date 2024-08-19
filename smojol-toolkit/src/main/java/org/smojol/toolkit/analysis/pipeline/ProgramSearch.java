package org.smojol.toolkit.analysis.pipeline;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.flowchart.ConsoleColors;

import java.io.File;
import java.util.Collection;
import java.util.List;

import static org.apache.commons.lang3.tuple.ImmutablePair.nullPair;

public class ProgramSearch {
    public static final ImmutablePair<File, String> NO_PATH = ImmutablePair.nullPair();

    public Pair<File, String> run(String program, String sourceDir) {
        System.out.println("Searching for program: " + program + "...");
        Collection<File> files = FileUtils.listFiles(new File(sourceDir), null, true);
        List<File> matchingFiles = files.stream().filter(f -> program.equals(f.getName())).toList();
        if (matchingFiles.isEmpty()) {
            System.out.println(ConsoleColors.red("No files matching " + program + " in " + sourceDir + ". Terminating recursion..."));
            ImmutablePair<File, String> empty = nullPair();
            return empty;
        }
        File foundFile = matchingFiles.getFirst();
        String srcDir = foundFile.getParent();
        System.out.println("Found " + foundFile.getName() + " in " + srcDir);

        return ImmutablePair.of(foundFile, srcDir);
    }
}
