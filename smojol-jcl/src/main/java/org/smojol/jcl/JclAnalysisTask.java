package org.smojol.jcl;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import org.smojol.jcl.model.JclParseResult;
import org.smojol.jcl.model.JclParsingException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * Task for analyzing JCL files.
 * This class handles batch processing of JCL files and can export results.
 */
public class JclAnalysisTask {
    private static final Logger LOGGER = Logger.getLogger(JclAnalysisTask.class.getName());
    
    private final JclParserService parserService;
    private final Gson gson;
    
    /**
     * Constructor.
     *
     * @param parserService the JCL parser service
     */
    public JclAnalysisTask(JclParserService parserService) {
        this.parserService = parserService;
        this.gson = new GsonBuilder().setPrettyPrinting().disableHtmlEscaping().create();
    }
    
    /**
     * Analyze a single JCL file and export the result to JSON.
     *
     * @param jclFilePath path to the JCL file
     * @param outputPath path where to write the JSON output
     * @throws JclParsingException if parsing or writing fails
     */
    public void analyzeAndExport(Path jclFilePath, Path outputPath) throws JclParsingException {
        LOGGER.info("Analyzing JCL file: " + jclFilePath);
        
        JclParseResult result = parserService.parseJclFile(jclFilePath);
        
        try {
            // Ensure output directory exists
            Files.createDirectories(outputPath.getParent());
            
            // Write JSON output
            String json = gson.toJson(result);
            Files.writeString(outputPath, json);
            
            LOGGER.info("JCL analysis exported to: " + outputPath);
            
        } catch (IOException e) {
            throw new JclParsingException("Failed to write analysis output", e);
        }
    }
    
    /**
     * Analyze multiple JCL files in a directory.
     *
     * @param jclDirectory directory containing JCL files
     * @param outputDirectory directory where to write JSON outputs
     * @param recursive whether to search subdirectories
     * @return list of successfully parsed JCL files
     * @throws JclParsingException if directory operations fail
     */
    public List<Path> analyzeBatch(Path jclDirectory, Path outputDirectory, boolean recursive) 
            throws JclParsingException {
        LOGGER.info("Starting batch JCL analysis in: " + jclDirectory);
        
        if (!Files.isDirectory(jclDirectory)) {
            throw new JclParsingException("Not a directory: " + jclDirectory);
        }
        
        List<Path> successfullyParsed = new ArrayList<>();
        List<Path> jclFiles = findJclFiles(jclDirectory, recursive);
        
        LOGGER.info("Found " + jclFiles.size() + " JCL files to analyze");
        
        for (Path jclFile : jclFiles) {
            try {
                String fileName = jclFile.getFileName().toString();
                String jsonFileName = fileName.replaceAll("\\.(jcl|JCL)$", ".json");
                Path outputPath = outputDirectory.resolve(jsonFileName);
                
                analyzeAndExport(jclFile, outputPath);
                successfullyParsed.add(jclFile);
                
            } catch (Exception e) {
                LOGGER.severe("Failed to analyze JCL file: " + jclFile + ". Error: " + e.getMessage());
            }
        }
        
        LOGGER.info("Batch analysis completed. Successfully parsed " + successfullyParsed.size() + 
                "/" + jclFiles.size() + " files");
        
        return successfullyParsed;
    }
    
    /**
     * Find all JCL files in a directory.
     *
     * @param directory the directory to search
     * @param recursive whether to search subdirectories
     * @return list of JCL files found
     * @throws JclParsingException if directory traversal fails
     */
    private List<Path> findJclFiles(Path directory, boolean recursive) throws JclParsingException {
        List<Path> jclFiles = new ArrayList<>();
        
        try {
            if (recursive) {
                Files.walk(directory)
                        .filter(Files::isRegularFile)
                        .filter(this::isJclFile)
                        .forEach(jclFiles::add);
            } else {
                Files.list(directory)
                        .filter(Files::isRegularFile)
                        .filter(this::isJclFile)
                        .forEach(jclFiles::add);
            }
        } catch (IOException e) {
            throw new JclParsingException("Failed to list JCL files in directory", e);
        }
        
        return jclFiles;
    }
    
    /**
     * Check if a file is a JCL file based on extension.
     *
     * @param path the file path
     * @return true if it's a JCL file
     */
    private boolean isJclFile(Path path) {
        String fileName = path.getFileName().toString().toLowerCase();
        return fileName.endsWith(".jcl") || fileName.endsWith(".jcl");
    }
    
    /**
     * Parse a JCL file and return it as a formatted JSON string.
     *
     * @param jclFilePath path to the JCL file
     * @return formatted JSON string
     * @throws JclParsingException if parsing fails
     */
    public String parseToJsonString(Path jclFilePath) throws JclParsingException {
        JclParseResult result = parserService.parseJclFile(jclFilePath);
        return gson.toJson(result);
    }
    
    /**
     * Parse a JCL file and return only the JCL structure (without metadata).
     *
     * @param jclFilePath path to the JCL file
     * @return JsonObject containing only the JCL structure
     * @throws JclParsingException if parsing fails
     */
    public JsonObject parseJclStructure(Path jclFilePath) throws JclParsingException {
        return parserService.parseJclToJson(jclFilePath);
    }
}
