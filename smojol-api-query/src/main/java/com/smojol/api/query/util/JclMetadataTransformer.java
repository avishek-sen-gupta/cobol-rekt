package com.smojol.api.query.util;

import com.smojol.api.query.model.DdStatement;
import com.smojol.api.query.model.JCLFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Transforme les métadonnées JCL (format JSON Map) en objets JCL exploitables
 * Utilisé par ASTParser pour peupler les steps avec DD statements
 */
public class JclMetadataTransformer {
    private static final Logger logger = LoggerFactory.getLogger(JclMetadataTransformer.class);

    /**
     * Transforme jclExecutionContext en List de JCLStep avec DD statements
     *
     * @param jclExecutionContext Map extraite de root.get("jclExecutionContext")
     * @return List de JCLStep objects avec DD statements parsés
     */
    public List<JCLFile.JCLStep> transformSteps(Map<String, Object> jclExecutionContext) {
        List<JCLFile.JCLStep> result = new ArrayList<>();

        if (jclExecutionContext == null || jclExecutionContext.isEmpty()) {
            logger.debug("No JCL execution context to transform");
            return result;
        }

        Object stepsObj = jclExecutionContext.get("steps");
        if (!(stepsObj instanceof List)) {
            logger.debug("No steps array found in jclExecutionContext");
            return result;
        }

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> stepsList = (List<Map<String, Object>>) stepsObj;

        for (Map<String, Object> stepData : stepsList) {
            try {
                JCLFile.JCLStep step = transformSingleStep(stepData);
                result.add(step);
                logger.trace("Transformed JCL step: {}", step.getName());
            } catch (Exception e) {
                logger.warn("Failed to transform JCL step: {}", e.getMessage(), e);
            }
        }

        logger.debug("Transformed {} JCL steps", result.size());
        return result;
    }

    /**
     * Transforme une seule étape JCL en JCLStep object
     *
     * @param stepData Map contenant name, parameters, dd_statements, etc.
     * @return JCLStep object peuplé avec DD statements
     */
    private JCLFile.JCLStep transformSingleStep(Map<String, Object> stepData) {
        String stepName = getString(stepData, "name", "");
        
        // Extraire le nom du programme depuis parameters.PGM
        String programName = "";
        Object parametersObj = stepData.get("parameters");
        if (parametersObj instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> parametersMap = (Map<String, Object>) parametersObj;
            Object pgmValue = parametersMap.get("PGM");
            if (pgmValue != null) {
                programName = pgmValue.toString();
            }
        }

        // Extraire et transformer les DD statements
        List<DdStatement> ddStatements = transformDdStatements(stepData);

        // Extraire datasets (liste simple de noms)
        List<String> datasets = extractDatasetNames(ddStatements);

        // Extraire parameters (si présents) - convertir en Map<String, String>
        @SuppressWarnings("unchecked")
        Map<String, Object> paramsObj = (Map<String, Object>) stepData.getOrDefault("parameters", Map.of());
        Map<String, String> parameters = new java.util.HashMap<>();
        for (Map.Entry<String, Object> entry : paramsObj.entrySet()) {
            parameters.put(entry.getKey(), entry.getValue() != null ? entry.getValue().toString() : "");
        }

        return JCLFile.JCLStep.builder()
                .name(stepName)
                .program(programName)
                .ddStatements(ddStatements)
                .datasets(datasets)
                .parameters(parameters)
                .build();
    }

    /**
     * Transforme les DD statements d'une étape
     *
     * @param stepData Map contenant dd_statements array
     * @return List de DdStatement objects parsés
     */
    private List<DdStatement> transformDdStatements(Map<String, Object> stepData) {
        List<DdStatement> result = new ArrayList<>();

        Object ddStatementsObj = stepData.get("dd_statements");
        if (!(ddStatementsObj instanceof List)) {
            return result;
        }

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> ddStatementsList = (List<Map<String, Object>>) ddStatementsObj;

        for (Map<String, Object> ddData : ddStatementsList) {
            try {
                DdStatement ddStatement = transformSingleDdStatement(ddData);
                result.add(ddStatement);
                logger.trace("Transformed DD statement: {}", ddStatement.getName());
            } catch (Exception e) {
                logger.warn("Failed to transform DD statement: {}", e.getMessage());
            }
        }

        return result;
    }

    /**
     * Transforme un seul DD statement
     *
     * @param ddData Map contenant name, parameters, line
     * @return DdStatement object avec paramètres parsés automatiquement
     */
    private DdStatement transformSingleDdStatement(Map<String, Object> ddData) {
        String name = getString(ddData, "name", "");
        int lineNumber = getInt(ddData, "line", 0);

        @SuppressWarnings("unchecked")
        Map<String, Object> parameters = (Map<String, Object>) ddData.getOrDefault("parameters", Map.of());

        DdStatement ddStatement = DdStatement.builder()
                .name(name)
                .parameters(parameters)
                .lineNumber(lineNumber)
                .build();

        // Parse automatique des paramètres pour accès facile
        ddStatement.parseParameters();

        return ddStatement;
    }

    /**
     * Extrait la liste des noms de datasets depuis les DD statements
     */
    private List<String> extractDatasetNames(List<DdStatement> ddStatements) {
        List<String> datasets = new ArrayList<>();
        
        for (DdStatement dd : ddStatements) {
            String dsn = dd.getParameter("DSN");
            if (dsn != null && !dsn.isEmpty()) {
                datasets.add(dsn);
            }
        }
        
        return datasets;
    }

    // ==================== Utilitaires de casting sécurisé ====================

    /**
     * Récupère une String depuis la Map de manière sécurisée
     */
    private String getString(Map<String, Object> map, String key, String defaultValue) {
        if (map == null) {
            return defaultValue;
        }
        Object value = map.get(key);
        if (value instanceof String) {
            return (String) value;
        }
        return defaultValue;
    }

    /**
     * Récupère un Integer depuis la Map de manière sécurisée
     */
    private int getInt(Map<String, Object> map, String key, int defaultValue) {
        if (map == null) {
            return defaultValue;
        }
        Object value = map.get(key);
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        return defaultValue;
    }
}
