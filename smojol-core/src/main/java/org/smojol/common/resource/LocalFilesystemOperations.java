package org.smojol.common.resource;

import org.apache.commons.io.FileUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;

public class LocalFilesystemOperations implements ResourceOperations {
    @Override
    public Collection<File> listFiles(File directory) {
        return FileUtils.listFiles(directory, null, true);
    }

    @Override
    public void createDirectories(Path path) throws IOException {
        Files.createDirectories(path);
    }

    @Override
    public Writer fileWriter(String path) throws IOException {
        return new FileWriter(path);
    }

    @Override
    public PrintWriter printWriter(String outputPath) throws IOException {
        return new PrintWriter(outputPath, StandardCharsets.UTF_8);
    }

    @Override
    public byte[] readAllBytes(Path path) throws IOException {
        return Files.readAllBytes(path);
    }
}
