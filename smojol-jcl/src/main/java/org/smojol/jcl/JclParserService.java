package org.smojol.jcl;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.smojol.jcl.model.JclParseResult;
import org.smojol.jcl.model.JclParsingException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Service for parsing JCL files using the Python legacylens-jcl-parser library.
 * This class acts as a bridge between Java and the Python parser.
 */
public class JclParserService {
    private static final Logger LOGGER = Logger.getLogger(JclParserService.class.getName());
    
    private static final String PYTHON_SCRIPT = "jcl_parser_wrapper.py";
    private static final int TIMEOUT_SECONDS = 30;
    
    private final String pythonExecutable;
    private final Path pythonScriptPath;
    private final Gson gson;
    
    /**
     * Constructor with default Python executable (python).
     *
     * @throws JclParsingException if Python script cannot be found
     */
    public JclParserService() throws JclParsingException {
        this("python");
    }
    
    /**
     * Constructor with custom Python executable.
     *
     * @param pythonExecutable path to Python executable (e.g., "python3", "python", "/usr/bin/python3")
     * @throws JclParsingException if Python script cannot be found
     */
    public JclParserService(String pythonExecutable) throws JclParsingException {
        this.pythonExecutable = pythonExecutable;
        this.gson = new com.google.gson.GsonBuilder().disableHtmlEscaping().create();
        this.pythonScriptPath = locatePythonScript();
        
        LOGGER.info("JCL Parser Service initialized with Python: " + pythonExecutable + 
                " and script: " + pythonScriptPath);
    }
    
    /**
     * Locate the Python script in the classpath or filesystem.
     *
     * @return Path to the Python script
     * @throws JclParsingException if script cannot be found
     */
    private Path locatePythonScript() throws JclParsingException {
        // Try to find script in resources (packaged in JAR)
        try (InputStream is = getClass().getClassLoader()
                .getResourceAsStream("python/" + PYTHON_SCRIPT)) {
            if (is != null) {
                // Extract to temp directory
                Path tempScript = Files.createTempFile("jcl_parser_wrapper", ".py");
                Files.copy(is, tempScript, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                tempScript.toFile().deleteOnExit();
                LOGGER.fine("Using Python script from resources: " + tempScript);
                return tempScript;
            }
        } catch (IOException e) {
            LOGGER.warning("Failed to extract Python script from resources: " + e.getMessage());
        }
        
        // Try to find script in project structure (development mode)
        Path projectScript = Paths.get("smojol-jcl", "python", PYTHON_SCRIPT);
        if (Files.exists(projectScript)) {
            LOGGER.fine("Using Python script from project: " + projectScript);
            return projectScript;
        }
        
        // Try current directory
        Path currentDirScript = Paths.get("python", PYTHON_SCRIPT);
        if (Files.exists(currentDirScript)) {
            LOGGER.fine("Using Python script from current directory: " + currentDirScript);
            return currentDirScript;
        }
        
        throw new JclParsingException("Could not locate Python script: " + PYTHON_SCRIPT);
    }
    
    /**
     * Parse a JCL file and return the parsed structure.
     *
     * @param jclFilePath path to the JCL file
     * @return JclParseResult containing the parsed structure
     * @throws JclParsingException if parsing fails
     */
    public JclParseResult parseJclFile(Path jclFilePath) throws JclParsingException {
        if (!Files.exists(jclFilePath)) {
            throw new JclParsingException("JCL file not found: " + jclFilePath);
        }
        
        LOGGER.info("Parsing JCL file: " + jclFilePath);
        
        try {
            // Build the process
            ProcessBuilder processBuilder = new ProcessBuilder(
                    pythonExecutable,
                    pythonScriptPath.toAbsolutePath().toString(),
                    jclFilePath.toAbsolutePath().toString()
            );
            
            processBuilder.redirectErrorStream(true);
            
            // Start the process
            Process process = processBuilder.start();
            
            // Read the output
            String output = readProcessOutput(process.getInputStream());
            
            // Wait for process to complete
            boolean completed = process.waitFor(TIMEOUT_SECONDS, TimeUnit.SECONDS);
            
            if (!completed) {
                process.destroyForcibly();
                throw new JclParsingException("Python parser timed out after " + TIMEOUT_SECONDS + " seconds");
            }
            
            int exitCode = process.exitValue();
            LOGGER.fine("Python parser exit code: " + exitCode);
            LOGGER.fine("Python parser output: " + output);
            
            // Parse the JSON output
            JclParseResult result = gson.fromJson(output, JclParseResult.class);
            
            if (result.isError()) {
                throw new JclParsingException("JCL parsing failed: " + result.getErrorDetails());
            }
            
            LOGGER.info("Successfully parsed JCL file: " + jclFilePath);
            return result;
            
        } catch (IOException e) {
            throw new JclParsingException("Failed to execute Python parser: " + e.getMessage(), e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new JclParsingException("Python parser was interrupted: " + e.getMessage(), e);
        } catch (Exception e) {
            throw new JclParsingException("Unexpected error during JCL parsing: " + e.getMessage(), e);
        }
    }
    
    /**
     * Parse a JCL file and return the parsed structure.
     *
     * @param jclFilePath path to the JCL file as a string
     * @return JclParseResult containing the parsed structure
     * @throws JclParsingException if parsing fails
     */
    public JclParseResult parseJclFile(String jclFilePath) throws JclParsingException {
        return parseJclFile(Paths.get(jclFilePath));
    }
    
    /**
     * Parse a JCL file and return the parsed structure as JsonObject.
     *
     * @param jclFilePath path to the JCL file
     * @return JsonObject containing the parsed JCL structure
     * @throws JclParsingException if parsing fails
     */
    public JsonObject parseJclToJson(Path jclFilePath) throws JclParsingException {
        JclParseResult result = parseJclFile(jclFilePath);
        return result.getJcl();
    }
    
    /**
     * Read the output from a process input stream.
     *
     * @param inputStream the input stream to read from
     * @return the output as a string
     * @throws IOException if reading fails
     */
    private String readProcessOutput(InputStream inputStream) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
            return reader.lines().collect(Collectors.joining("\n"));
        }
    }
    
    /**
     * Check if the Python environment is properly configured.
     *
     * @return true if Python and the parser library are available
     */
    public boolean checkEnvironment() {
        try {
            ProcessBuilder pb = new ProcessBuilder(pythonExecutable, "--version");
            Process process = pb.start();
            boolean completed = process.waitFor(5, TimeUnit.SECONDS);
            
            if (!completed || process.exitValue() != 0) {
                LOGGER.warning("Python executable not found or invalid: " + pythonExecutable);
                return false;
            }
            
            // Check if legacylens-jcl-parser is installed
            pb = new ProcessBuilder(pythonExecutable, "-c", 
                    "import legacylens_jcl_parser; print('OK')");
            process = pb.start();
            String output = readProcessOutput(process.getInputStream());
            completed = process.waitFor(5, TimeUnit.SECONDS);
            
            if (!completed || process.exitValue() != 0 || !output.contains("OK")) {
                LOGGER.warning("legacylens-jcl-parser library not installed");
                return false;
            }
            
            LOGGER.info("Python environment is properly configured");
            return true;
            
        } catch (Exception e) {
            LOGGER.severe("Failed to check Python environment: " + e.getMessage());
            return false;
        }
    }
}
