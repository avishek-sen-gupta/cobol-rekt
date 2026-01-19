package com.smojol.api.query.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.smojol.api.query.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Parser pour convertir les fichiers AST JSON en data models
 */
public class ASTParser {
    private static final Logger logger = LoggerFactory.getLogger(ASTParser.class);
    private static final ObjectMapper mapper = new ObjectMapper();

    /**
     * Parse un fichier COBOL AST JSON
     */
    public static CBLFile parseCbl(String json, String fileName) {
        try {
            JsonNode root = mapper.readTree(json);
            
            CBLFile.CBLFileBuilder builder = CBLFile.builder();
            
            // Extraire le nom du programme
            String name = extractProgramName(fileName);
            builder.name(name);
            
            // Copier les métadonnées
            builder.path(getStringValue(root, "path", ""));
            builder.programId(getStringValue(root, "program_id", name));
            builder.size(json.length());
            builder.lines(countLines(json));
            builder.parseStatus(ParseStatus.SUCCESS);
            builder.lastModified(System.currentTimeMillis());
            
            // Extraire les dépendances
            builder.copybooks(extractCopybooks(root));
            builder.datasets(extractDatasets(root));
            builder.callees(extractCallees(root));
            builder.callers(extractCallers(root));
            
            // Stocker les données AST complètes
            builder.astData(mapper.convertValue(root, Map.class));
            
            // Construire le CBLFile
            CBLFile cblFile = builder.build();
            
            // NOUVEAU : Transformer copybooksMetadata en Copybook objects
            // et peupler copybooksList
            try {
                @SuppressWarnings("unchecked")
                Map<String, Object> copybooksMetadataMap =
                    (Map<String, Object>) mapper.convertValue(
                        root.get("copybooksMetadata"),
                        Map.class
                    );

                if (copybooksMetadataMap != null && !copybooksMetadataMap.isEmpty()) {
                    CopybookMetadataTransformer transformer =
                        new CopybookMetadataTransformer();
                    List<Copybook> copybooksList = transformer.transform(copybooksMetadataMap);

                    cblFile.setCopybooksList(copybooksList);
                    logger.debug("Populated copybooksList with {} copybooks for: {}",
                        copybooksList.size(), name);
                } else {
                    logger.debug("No copybooksMetadata found for: {}", name);
                }
            } catch (Exception e) {
                logger.warn("Error transforming copybooksMetadata for {}: {}",
                    name, e.getMessage());
            }
            
            return cblFile;
            
        } catch (Exception e) {
            logger.error("Error parsing CBL AST for {}: {}", fileName, e.getMessage(), e);
            return CBLFile.builder()
                    .name(extractProgramName(fileName))
                    .path(fileName)
                    .parseStatus(ParseStatus.ERROR)
                    .parseMessage(e.getMessage())
                    .lastModified(System.currentTimeMillis())
                    .build();
        }
    }

    /**
     * Parse un fichier JCL AST JSON
     */
    public static JCLFile parseJcl(String json, String fileName) {
        try {
            JsonNode root = mapper.readTree(json);
            
            JCLFile.JCLFileBuilder builder = JCLFile.builder();
            
            String name = extractProgramName(fileName);
            builder.name(name);
            builder.jobName(getStringValue(root, "job_name", name));
            builder.path(getStringValue(root, "path", ""));
            builder.size(json.length());
            builder.lines(countLines(json));
            builder.parseStatus(ParseStatus.SUCCESS);
            builder.lastModified(System.currentTimeMillis());
            
            // NOUVEAU: Extraire depuis jclExecutionContext si disponible
            List<String> programs = new ArrayList<>();
            List<String> datasets = new ArrayList<>();
            
            try {
                JsonNode jclExecutionContextNode = root.get("jclExecutionContext");
                if (jclExecutionContextNode != null) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> jclExecutionContext = 
                        mapper.convertValue(jclExecutionContextNode, Map.class);
                    
                    if (jclExecutionContext != null && !jclExecutionContext.isEmpty()) {
                        JclMetadataTransformer transformer = new JclMetadataTransformer();
                        List<JCLFile.JCLStep> steps = transformer.transformSteps(jclExecutionContext);
                        builder.steps(steps);
                        
                        // Extraire programmes et datasets depuis les steps
                        for (JCLFile.JCLStep step : steps) {
                            if (step.getProgram() != null && !step.getProgram().isEmpty()) {
                                programs.add(step.getProgram());
                            }
                            if (step.getDatasets() != null) {
                                datasets.addAll(step.getDatasets());
                            }
                        }
                        
                        logger.debug("Populated {} JCL steps with DD statements for: {}", 
                            steps.size(), name);
                        logger.debug("Found {} programs and {} datasets from jclExecutionContext", 
                            programs.size(), datasets.size());
                    } else {
                        logger.debug("No jclExecutionContext found, falling back to simple extraction");
                        builder.steps(extractJclSteps(root));
                        programs = extractProgramsFromJcl(root);
                        datasets = extractDatasetsFromJcl(root);
                    }
                } else {
                    logger.debug("No jclExecutionContext node, falling back to simple extraction");
                    builder.steps(extractJclSteps(root));
                    programs = extractProgramsFromJcl(root);
                    datasets = extractDatasetsFromJcl(root);
                }
            } catch (Exception e) {
                logger.warn("Error transforming jclExecutionContext for {}: {}, falling back to simple extraction", 
                    name, e.getMessage());
                builder.steps(extractJclSteps(root));
                programs = extractProgramsFromJcl(root);
                datasets = extractDatasetsFromJcl(root);
            }
            
            builder.programs(programs);
            builder.datasets(datasets);
            
            // Stocker les données JCL
            builder.jclData(mapper.convertValue(root, Map.class));
            
            return builder.build();
            
        } catch (Exception e) {
            logger.error("Error parsing JCL AST for {}: {}", fileName, e.getMessage(), e);
            return JCLFile.builder()
                    .name(extractProgramName(fileName))
                    .path(fileName)
                    .parseStatus(ParseStatus.ERROR)
                    .parseMessage(e.getMessage())
                    .lastModified(System.currentTimeMillis())
                    .build();
        }
    }

    /**
     * Parse un fichier Copybook AST JSON
     */
    public static Copybook parseCopybook(String json, String fileName) {
        try {
            JsonNode root = mapper.readTree(json);
            
            Copybook.CopybookBuilder builder = Copybook.builder();
            
            String name = extractProgramName(fileName);
            builder.name(name);
            builder.path(getStringValue(root, "path", ""));
            builder.size(json.length());
            builder.lines(countLines(json));
            builder.parseStatus(ParseStatus.SUCCESS);
            builder.lastModified(System.currentTimeMillis());
            
            // Extraire les includes (copybooks utilisés)
            builder.includes(extractIncludes(root));
            builder.usedByCobol(new ArrayList<>());  // Rempli par le service
            builder.usedByCopybook(new ArrayList<>());  // Rempli par le service
            
            // Extraire la structure de données
            builder.dataStructure(extractDataStructure(root));
            
            // Stocker les données complètes
            builder.cpyData(mapper.convertValue(root, Map.class));
            
            return builder.build();
            
        } catch (Exception e) {
            logger.error("Error parsing Copybook AST for {}: {}", fileName, e.getMessage(), e);
            return Copybook.builder()
                    .name(extractProgramName(fileName))
                    .path(fileName)
                    .parseStatus(ParseStatus.ERROR)
                    .parseMessage(e.getMessage())
                    .lastModified(System.currentTimeMillis())
                    .build();
        }
    }

    /**
     * Parse un Dataset JSON
     */
    public static Dataset parseDataset(String json, String datasetName) {
        try {
            JsonNode root = mapper.readTree(json);
            
            Dataset.DatasetBuilder builder = Dataset.builder();
            
            builder.name(datasetName);
            builder.type(getStringValue(root, "type", "SEQUENTIAL"));
            builder.organization(getStringValue(root, "organization", "SEQUENTIAL"));
            builder.recordFormat(getStringValue(root, "record_format", "FB"));
            builder.lrecl(getIntValue(root, "lrecl", 80));
            builder.blksize(getIntValue(root, "blksize", 800));
            builder.accessMethod(getStringValue(root, "access_method", "SEQ"));
            builder.volumeSerial(getStringValue(root, "volume_serial", ""));
            builder.unit(getStringValue(root, "unit", "SYSDA"));
            builder.retention(getStringValue(root, "retention", ""));
            builder.description(getStringValue(root, "description", ""));
            builder.lastModified(System.currentTimeMillis());
            
            builder.usedByCobol(new ArrayList<>());  // Rempli par le service
            builder.usedByJcl(new ArrayList<>());  // Rempli par le service
            
            return builder.build();
            
        } catch (Exception e) {
            logger.error("Error parsing Dataset for {}: {}", datasetName, e.getMessage(), e);
            return Dataset.builder()
                    .name(datasetName)
                    .organization("UNKNOWN")
                    .build();
        }
    }

    // ==================== Extracteurs Privés ====================

    private static String extractProgramName(String fileName) {
        return fileName.replace("-aggregated.json", "").replace(".json", "");
    }

    private static List<String> extractCopybooks(JsonNode root) {
        List<String> copybooks = new ArrayList<>();
        JsonNode cpyArray = root.get("copybooks");
        if (cpyArray != null && cpyArray.isArray()) {
            cpyArray.forEach(node -> copybooks.add(node.asText()));
        }
        return copybooks;
    }

    private static List<String> extractDatasets(JsonNode root) {
        List<String> datasets = new ArrayList<>();
        JsonNode dsArray = root.get("datasets");
        if (dsArray != null && dsArray.isArray()) {
            dsArray.forEach(node -> datasets.add(node.asText()));
        }
        return datasets;
    }

    private static List<String> extractCallees(JsonNode root) {
        List<String> callees = new ArrayList<>();
        findCallStatements(root, callees);
        
        // Dédupliquer et trier
        return callees.stream()
                .distinct()
                .sorted()
                .collect(Collectors.toList());
    }

    private static List<String> extractCallers(JsonNode root) {
        // Les callers ne peuvent pas être extraits directement de l'AST d'un programme
        // Ils doivent être calculés par analyse inverse dans SimpleASTQueryService
        return new ArrayList<>();
    }

    /**
     * Parcourt récursivement l'AST pour trouver tous les CallStatementContext
     */
    private static void findCallStatements(JsonNode node, List<String> callees) {
        if (node == null) {
            return;
        }
        
        if (node.isObject()) {
            // Vérifier si c'est un CallStatementContext
            JsonNode nodeType = node.get("nodeType");
            if (nodeType != null && "CallStatementContext".equals(nodeType.asText())) {
                String callTarget = extractCallTarget(node);
                if (callTarget != null && !callTarget.isEmpty()) {
                    callees.add(callTarget);
                    logger.debug("Found CALL to: {}", callTarget);
                }
            }
            
            // Parcourir tous les enfants récursivement
            Iterator<Map.Entry<String, JsonNode>> fields = node.fields();
            while (fields.hasNext()) {
                Map.Entry<String, JsonNode> field = fields.next();
                findCallStatements(field.getValue(), callees);
            }
        } else if (node.isArray()) {
            for (JsonNode child : node) {
                findCallStatements(child, callees);
            }
        }
    }

    /**
     * Extrait la cible d'un CALL depuis un CallStatementContext
     * Cherche dans les children pour trouver le literal ou identifier
     */
    private static String extractCallTarget(JsonNode callNode) {
        JsonNode children = callNode.get("children");
        if (children == null || !children.isArray()) {
            return null;
        }
        
        for (JsonNode child : children) {
            if (child == null || !child.isObject()) {
                continue;
            }
            
            JsonNode nodeType = child.get("nodeType");
            if (nodeType == null) {
                continue;
            }
            
            String type = nodeType.asText();
            
            // Chercher dans ConstantNameContext (CALL 'LITERAL')
            if (type.contains("ConstantName") || type.contains("Constant")) {
                String target = extractFromLiteralNode(child);
                if (target != null) {
                    return target;
                }
            }
            
            // Chercher dans IdentifierContext (CALL VARIABLE)
            if (type.contains("Identifier")) {
                JsonNode text = child.get("text");
                if (text != null) {
                    return text.asText();
                }
            }
        }
        
        return null;
    }

    /**
     * Extrait la valeur d'un literal depuis un node (récursif pour gérer LiteralContext imbriqués)
     */
    private static String extractFromLiteralNode(JsonNode node) {
        JsonNode text = node.get("text");
        if (text != null) {
            String value = text.asText();
            // Retirer les quotes
            if (value.startsWith("'") && value.endsWith("'")) {
                return value.substring(1, value.length() - 1);
            }
            if (value.startsWith("\"") && value.endsWith("\"")) {
                return value.substring(1, value.length() - 1);
            }
            return value;
        }
        
        // Chercher récursivement dans les children
        JsonNode children = node.get("children");
        if (children != null && children.isArray()) {
            for (JsonNode child : children) {
                String result = extractFromLiteralNode(child);
                if (result != null) {
                    return result;
                }
            }
        }
        
        return null;
    }

    private static List<String> extractProgramsFromJcl(JsonNode root) {
        List<String> programs = new ArrayList<>();
        JsonNode progArray = root.get("programs");
        if (progArray != null && progArray.isArray()) {
            progArray.forEach(node -> programs.add(node.asText()));
        }
        return programs;
    }

    private static List<String> extractDatasetsFromJcl(JsonNode root) {
        List<String> datasets = new ArrayList<>();
        JsonNode dsArray = root.get("datasets");
        if (dsArray != null && dsArray.isArray()) {
            dsArray.forEach(node -> datasets.add(node.asText()));
        }
        return datasets;
    }

    private static List<JCLFile.JCLStep> extractJclSteps(JsonNode root) {
        List<JCLFile.JCLStep> steps = new ArrayList<>();
        JsonNode stepsArray = root.get("steps");
        if (stepsArray != null && stepsArray.isArray()) {
            stepsArray.forEach(stepNode -> {
                JCLFile.JCLStep step = JCLFile.JCLStep.builder()
                        .name(getStringValue(stepNode, "name", ""))
                        .program(getStringValue(stepNode, "program", ""))
                        .datasets(extractDatasetsFromObject(stepNode))
                        .parameters(extractParameters(stepNode))
                        .build();
                steps.add(step);
            });
        }
        return steps;
    }

    private static List<String> extractIncludes(JsonNode root) {
        List<String> includes = new ArrayList<>();
        JsonNode includesArray = root.get("includes");
        if (includesArray != null && includesArray.isArray()) {
            includesArray.forEach(node -> includes.add(node.asText()));
        }
        return includes;
    }

    private static Map<String, Object> extractDataStructure(JsonNode root) {
        JsonNode dataNode = root.get("data_structure");
        if (dataNode != null) {
            return mapper.convertValue(dataNode, Map.class);
        }
        return new HashMap<>();
    }

    private static List<String> extractDatasetsFromObject(JsonNode node) {
        List<String> datasets = new ArrayList<>();
        JsonNode dsArray = node.get("datasets");
        if (dsArray != null && dsArray.isArray()) {
            dsArray.forEach(n -> datasets.add(n.asText()));
        }
        return datasets;
    }

    private static Map<String, String> extractParameters(JsonNode node) {
        Map<String, String> params = new HashMap<>();
        JsonNode paramsNode = node.get("parameters");
        if (paramsNode != null && paramsNode.isObject()) {
            paramsNode.fields().forEachRemaining(entry -> 
                params.put(entry.getKey(), entry.getValue().asText()));
        }
        return params;
    }

    // ==================== Utilitaires ====================

    private static String getStringValue(JsonNode node, String field, String defaultValue) {
        if (node == null || node.get(field) == null) {
            return defaultValue;
        }
        JsonNode value = node.get(field);
        return value.isNull() ? defaultValue : value.asText();
    }

    private static int getIntValue(JsonNode node, String field, int defaultValue) {
        if (node == null || node.get(field) == null) {
            return defaultValue;
        }
        JsonNode value = node.get(field);
        return value.isNull() ? defaultValue : value.asInt();
    }

    private static int countLines(String text) {
        return text.split("\n", -1).length;
    }
}
