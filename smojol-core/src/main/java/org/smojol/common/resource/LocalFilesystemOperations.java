package org.smojol.common.resource;

import org.apache.commons.io.FileUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.logging.Logger;

public class LocalFilesystemOperations implements ResourceOperations {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(LocalFilesystemOperations.class.getName());
    @Override
    public Collection<File> listFiles(File directory) {
        LOGGER.info(String.format("Listing files recursively in directory: '%s'", directory.getAbsolutePath()));
        return FileUtils.listFiles(directory, null, true);
    }

    @Override
    public void createDirectories(Path path) throws IOException {
        LOGGER.info(String.format("Creating directory: '%s'", path));
        Files.createDirectories(path);
    }

    @Override
    public Writer fileWriter(String path) throws IOException {
        LOGGER.info(String.format("Creating FileWriter for path: '%s'", path));
        return new FileWriter(path);
    }

    @Override
    public PrintWriter printWriter(String outputPath) throws IOException {
        LOGGER.info(String.format("Creating PrintWriter for path: '%s'", outputPath));
        return new PrintWriter(outputPath, StandardCharsets.UTF_8);
    }

    @Override
    public byte[] readAllBytes(Path path) throws IOException {
        LOGGER.info(String.format("Reading bytes from path: '%s'", path.toAbsolutePath()));
        return Files.readAllBytes(path);
    }
}
