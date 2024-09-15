package org.smojol.common.resource;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.file.Path;
import java.util.Collection;

public interface ResourceOperations {
    Collection<File> listFiles(File directory);
    void createDirectories(Path path) throws IOException;
    Writer fileWriter(String path) throws IOException;
    PrintWriter printWriter(String outputPath) throws IOException;
    byte[] readAllBytes(Path path) throws IOException;
}
