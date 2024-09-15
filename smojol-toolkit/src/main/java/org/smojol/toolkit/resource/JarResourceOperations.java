package org.smojol.toolkit.resource;

import org.apache.commons.io.FileUtils;
import org.smojol.common.resource.ResourceOperations;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.file.Path;
import java.util.Collection;

public class JarResourceOperations implements ResourceOperations {
    @Override
    public Collection<File> listFiles(File directory) {
        return FileUtils.listFiles(directory, null, true);
    }

    @Override
    public void createDirectories(Path path) throws IOException {

    }

    @Override
    public Writer fileWriter(String path) throws IOException {
        return null;
    }

    @Override
    public PrintWriter printWriter(String outputPath) throws IOException {
        return null;
    }

    @Override
    public byte[] readAllBytes(Path path) throws IOException {
        return new byte[0];
    }
}
