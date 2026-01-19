package com.smojol.api.query.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.smojol.api.query.model.JCLFile;
import com.smojol.api.query.model.ParseStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Parser pour le fichier jcl-analysis.json
 * Ce fichier contient tous les JCL analysés, qu'ils soient ou non référencés par des programmes COBOL
 */
public class JclAnalysisParser {
    private static final Logger logger = LoggerFactory.getLogger(JclAnalysisParser.class);
    private final ObjectMapper mapper = new ObjectMapper();
    
    // Constantes pour la validation DSN
    private static final Set<String> EXCLUDED_DSN_VALUES = Set.of("*", "DUMMY");
    private static final String SYSOUT_PREFIX = "SYSOUT=";
    private static final int MIN_DSN_LENGTH = 3;

    /**
     * Parse le fichier jcl-analysis.json et retourne la liste de tous les JCL
     */
    public List<JCLFile> parseJclAnalysis(Path jclAnalysisPath) {
        List<JCLFile> jclFiles = new ArrayList<>();
        
        try {
            if (!Files.exists(jclAnalysisPath)) {
                logger.warn("JCL analysis file not found: {}", jclAnalysisPath);
                return jclFiles;
            }

            String content = Files.readString(jclAnalysisPath);
            JsonNode root = mapper.readTree(content);
            JsonNode jclFilesNode = root.get("jcl_files");
            
            if (jclFilesNode == null || !jclFilesNode.isArray()) {
                logger.warn("No jcl_files array found in {}", jclAnalysisPath);
                return jclFiles;
            }

            for (JsonNode jclNode : jclFilesNode) {
                try {
                    JCLFile jclFile = parseJclEntry(jclNode);
                    jclFiles.add(jclFile);
                } catch (Exception e) {
                    logger.error("Error parsing JCL entry: {}", e.getMessage(), e);
                }
            }

            logger.info("Parsed {} JCL files from jcl-analysis.json", jclFiles.size());
            
        } catch (Exception e) {
            logger.error("Error parsing jcl-analysis.json: {}", e.getMessage(), e);
        }

        return jclFiles;
    }

    /**
     * Parse une entrée JCL individuelle du fichier jcl-analysis.json
     */
    private JCLFile parseJclEntry(JsonNode jclNode) {
        String name = getTextValue(jclNode, "stem", "");
        if (name.isEmpty()) {
            name = getTextValue(jclNode, "name", "").replace(".jcl", "");
        }

        String jobName = getTextValue(jclNode, "job_name", name);
        String path = getTextValue(jclNode, "path", "");

        // Extraire les programmes
        List<String> programs = new ArrayList<>();
        JsonNode programsNode = jclNode.get("programs");
        if (programsNode != null && programsNode.isArray()) {
            for (JsonNode programNode : programsNode) {
                programs.add(programNode.asText());
            }
        }

        // Extraire uniquement les DSN (vrais datasets) depuis les steps
        // Note: On ignore dd_names car ce sont les noms des DD statements (SYSPRINT, etc.)
        // pas les DSN (Data Set Names) qui sont les vrais datasets
        Set<String> datasetsSet = new HashSet<>();
        JsonNode stepsNode = jclNode.get("steps");
        List<JCLFile.JCLStep> steps = new ArrayList<>();
        if (stepsNode != null && stepsNode.isArray()) {
            for (JsonNode stepNode : stepsNode) {
                JCLFile.JCLStep step = parseStep(stepNode);
                steps.add(step);
                
                // Extraire les DSN des DD statements du step (vrais datasets)
                for (String dataset : step.getDatasets()) {
                    datasetsSet.add(dataset);
                }
            }
        }

        return JCLFile.builder()
                .name(name)
                .jobName(jobName)
                .path(path)
                .programs(programs)
                .datasets(new ArrayList<>(datasetsSet))
                .steps(steps)
                .parseStatus(ParseStatus.SUCCESS)
                .lastModified(System.currentTimeMillis())
                .build();
    }

    /**
     * Parse un step JCL
     */
    private JCLFile.JCLStep parseStep(JsonNode stepNode) {
        String stepName = getTextValue(stepNode, "name", "");
        String program = getTextValue(stepNode, "program", "");

        List<String> datasets = extractDatasetsFromDdStatements(stepNode);

        return JCLFile.JCLStep.builder()
                .name(stepName)
                .program(program)
                .datasets(datasets)
                .build();
    }

    /**
     * Extrait les datasets (DSN) depuis les DD statements d'un step
     */
    private List<String> extractDatasetsFromDdStatements(JsonNode stepNode) {
        List<String> datasets = new ArrayList<>();
        JsonNode ddStatementsNode = stepNode.get("dd_statements");
        
        if (ddStatementsNode != null && ddStatementsNode.isArray()) {
            for (JsonNode ddNode : ddStatementsNode) {
                extractDsnFromNode(ddNode, "dataset_info").ifPresent(datasets::add);
                extractDsnFromNode(ddNode, "parameters").ifPresent(datasets::add);
            }
        }
        
        return datasets;
    }

    /**
     * Extrait un DSN depuis un noeud JSON (dataset_info ou parameters)
     */
    private Optional<String> extractDsnFromNode(JsonNode parentNode, String childName) {
        JsonNode childNode = parentNode.get(childName);
        if (childNode == null) {
            return Optional.empty();
        }

        // Extraire le DSN selon le type de noeud
        String dsn = "dataset_info".equals(childName) 
            ? getTextValue(childNode, "dsn", "")
            : childNode.has("DSN") ? childNode.get("DSN").asText() : "";

        return isValidDsn(dsn) ? Optional.of(dsn) : Optional.empty();
    }

    /**
     * Valide qu'un DSN est un vrai dataset (pas SYSOUT, *, dummy, etc.)
     */
    private boolean isValidDsn(String dsn) {
        if (dsn == null || dsn.trim().isEmpty()) {
            return false;
        }
        
        String trimmed = dsn.trim();
        
        // Exclure les cas évidents de non-datasets
        if (EXCLUDED_DSN_VALUES.contains(trimmed) || 
            trimmed.startsWith(SYSOUT_PREFIX) ||
            trimmed.length() < MIN_DSN_LENGTH) {
            return false;
        }
        
        // Un DSN valide contient soit '.' (qualificateurs) soit '(' (membres PDS)
        // Ex: AWS.M2.CARDDEMO.LOADLIB ou &LBNM..CNTL(DB2FREE)
        return trimmed.contains(".") || trimmed.contains("(");
    }

    private String getTextValue(JsonNode node, String fieldName, String defaultValue) {
        JsonNode field = node.get(fieldName);
        if (field != null && !field.isNull()) {
            return field.asText();
        }
        return defaultValue;
    }
}
