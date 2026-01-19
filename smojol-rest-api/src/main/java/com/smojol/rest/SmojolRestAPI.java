package com.smojol.rest;

import com.smojol.api.query.service.ASTQueryService;
import com.smojol.api.query.service.SimpleASTQueryService;
import com.smojol.api.query.model.*;
import io.javalin.Javalin;
import io.javalin.http.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Path;

import java.util.*;
import java.util.stream.Collectors;

/**
 * REST API Server for COBOL Explorer
 * Exposes AST data via REST endpoints
 */
public class SmojolRestAPI {
    private static final Logger logger = LoggerFactory.getLogger(SmojolRestAPI.class);
    private static final int DEFAULT_PORT = 8080;
    
    private final ASTQueryService queryService;
    
    public SmojolRestAPI(ASTQueryService queryService) {
        this.queryService = queryService;
    }
    
    public void start(int port) {
        Javalin app = Javalin.create(config -> {
            config.plugins.enableCors(cors -> {
                cors.add(it -> {
                    it.anyHost();
                });
            });
        });
        
        // Health check
        app.get("/api/health", ctx -> {
            ctx.json(Map.of("status", "OK", "service", "SmojolRestAPI"));
        });
        
        // Get all COBOL programs
        app.get("/api/programs", this::getAllPrograms);
        
        // Get specific program
        app.get("/api/programs/{name}", this::getProgram);
        
        // Get all JCL files
        app.get("/api/jcls", this::getAllJcls);
        
        // Get specific JCL
        app.get("/api/jcls/{name}", this::getJcl);
        
        // Get all copybooks
        app.get("/api/copybooks", this::getAllCopybooks);
        
        // Get specific copybook
        app.get("/api/copybooks/{name}", this::getCopybook);
        
        // Get all datasets
        app.get("/api/datasets", this::getAllDatasets);
        
        // Get specific dataset
        app.get("/api/datasets/{name}", this::getDataset);
        
        app.start(port);
        logger.info("✅ SmojolRestAPI started on port {}", port);
        logger.info("🌐 API available at: http://localhost:{}/api", port);
    }
    
    private void getAllPrograms(Context ctx) {
        try {
            List<Map<String, Object>> programs = queryService.getAllCbl().stream()
                .map(this::cblToMap)
                .collect(Collectors.toList());
            ctx.json(programs);
        } catch (Exception e) {
            logger.error("Error getting all programs", e);
            ctx.status(500).json(Map.of("error", e.getMessage()));
        }
    }
    
    private void getProgram(Context ctx) {
        String name = ctx.pathParam("name");
        queryService.getCbl(name)
            .map(this::cblToMap)
            .ifPresentOrElse(
                ctx::json,
                () -> ctx.status(404).json(Map.of("error", "Program not found"))
            );
    }
    
    private void getAllJcls(Context ctx) {
        try {
            List<Map<String, Object>> jcls = queryService.getAllJcl().stream()
                .map(this::jclToMap)
                .collect(Collectors.toList());
            ctx.json(jcls);
        } catch (Exception e) {
            logger.error("Error getting all JCLs", e);
            ctx.status(500).json(Map.of("error", e.getMessage()));
        }
    }
    
    private void getJcl(Context ctx) {
        String name = ctx.pathParam("name");
        queryService.getJcl(name)
            .map(this::jclToMap)
            .ifPresentOrElse(
                ctx::json,
                () -> ctx.status(404).json(Map.of("error", "JCL not found"))
            );
    }
    
    private void getAllCopybooks(Context ctx) {
        try {
            List<Map<String, Object>> copybooks = queryService.getAllCopybooks().stream()
                .map(this::copybookToMap)
                .collect(Collectors.toList());
            ctx.json(copybooks);
        } catch (Exception e) {
            logger.error("Error getting all copybooks", e);
            ctx.status(500).json(Map.of("error", e.getMessage()));
        }
    }
    
    private void getCopybook(Context ctx) {
        String name = ctx.pathParam("name");
        queryService.getCopybook(name)
            .map(this::copybookToMap)
            .ifPresentOrElse(
                ctx::json,
                () -> ctx.status(404).json(Map.of("error", "Copybook not found"))
            );
    }
    
    private void getAllDatasets(Context ctx) {
        try {
            List<Map<String, Object>> datasets = queryService.getAllDatasets().stream()
                .map(this::datasetToMap)
                .collect(Collectors.toList());
            ctx.json(datasets);
        } catch (Exception e) {
            logger.error("Error getting all datasets", e);
            ctx.status(500).json(Map.of("error", e.getMessage()));
        }
    }
    
    private void getDataset(Context ctx) {
        String name = ctx.pathParam("name");
        queryService.getDataset(name)
            .map(this::datasetToMap)
            .ifPresentOrElse(
                ctx::json,
                () -> ctx.status(404).json(Map.of("error", "Dataset not found"))
            );
    }
    
    // Conversion methods
    private Map<String, Object> cblToMap(CBLFile cbl) {
        Map<String, Object> map = new HashMap<>();
        map.put("name", cbl.getName());
        map.put("path", cbl.getPath());
        map.put("copybooks", orEmpty(cbl.getCopybooks()));
        map.put("jcls", orEmpty(cbl.getJcls()));
        map.put("callees", orEmpty(cbl.getCallees()));
        map.put("callers", orEmpty(cbl.getCallers()));
        return map;
    }
    
    private Map<String, Object> jclToMap(JCLFile jcl) {
        Map<String, Object> map = new HashMap<>();
        map.put("name", jcl.getName());
        map.put("jobName", jcl.getJobName());
        map.put("programs", orEmpty(jcl.getPrograms()));
        map.put("datasets", orEmpty(jcl.getDatasets()));
        return map;
    }
    
    private Map<String, Object> copybookToMap(Copybook copybook) {
        Map<String, Object> map = new HashMap<>();
        map.put("name", copybook.getName());
        map.put("usedBy", orEmpty(copybook.getUsedByCobol()));
        return map;
    }
    
    private Map<String, Object> datasetToMap(Dataset dataset) {
        Map<String, Object> map = new HashMap<>();
        map.put("name", dataset.getName());
        map.put("organization", dataset.getOrganization());
        map.put("usedByJcls", orEmpty(dataset.getUsedByJcl()));
        map.put("usedByCobol", orEmpty(dataset.getUsedByCobol()));
        return map;
    }
    
    /**
     * Retourne une liste vide si la liste est null, sinon retourne la liste
     */
    private <T> List<T> orEmpty(List<T> list) {
        return list != null ? list : Collections.emptyList();
    }
    
    public static void main(String[] args) {
        // Get AST base path from system property or environment variable
        String astBasePath = System.getProperty("ast.base.path", 
            System.getenv().getOrDefault("AST_BASE_PATH", "./out"));
        
        logger.info("🚀 Starting SmojolRestAPI...");
        logger.info("📁 AST Base Path: {}", astBasePath);
        
        // Initialize service with real data
        SimpleASTQueryService queryService = new SimpleASTQueryService(astBasePath);
        
        // Preload all ASTs and build call graph
        logger.info("🔄 Preloading ASTs and building call graph...");
        queryService.preloadAllAndResolveIncludes();
        logger.info("✅ Preload complete. Call graph built.");
        
        // Start server
        int port = Integer.parseInt(System.getProperty("server.port", 
            String.valueOf(DEFAULT_PORT)));
        
        new SmojolRestAPI(queryService).start(port);
    }
}
