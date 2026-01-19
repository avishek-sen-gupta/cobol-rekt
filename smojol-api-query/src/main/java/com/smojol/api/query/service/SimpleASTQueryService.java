package com.smojol.api.query.service;

import com.smojol.api.query.config.ASTConfig;
import com.smojol.api.query.util.ASTLoader;
import com.smojol.api.query.util.JclAnalysisParser;
import com.smojol.api.query.model.CBLFile;
import com.smojol.api.query.model.Copybook;
import com.smojol.api.query.model.Dataset;
import com.smojol.api.query.model.JCLFile;
import com.smojol.api.query.model.ParseStatus;
import com.smojol.api.query.util.CopybookIncludesResolver;
import com.smojol.api.query.util.CycleDetector;
import com.smojol.api.query.util.SimpleCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;


/**
 * Implémentation simple du service de query AST
 * Utilise:
 * - ASTLoader pour charger les fichiers
 * - SimpleCache pour cacher en mémoire
 * - CycleDetector pour détecter les cycles copybook
 */
public class SimpleASTQueryService implements ASTQueryService {
    private static final Logger logger = LoggerFactory.getLogger(SimpleASTQueryService.class);

    private final ASTConfig config;
    private final ASTLoader loader;
    private final SimpleCache<String, CBLFile> cblCache;
    private final SimpleCache<String, JCLFile> jclCache;
    private final SimpleCache<String, Copybook> copybookCache;

    // Caches de programmes et datasets trouvés
    private Map<String, CBLFile> allCbls = new HashMap<>();
    private Map<String, JCLFile> allJcls = new HashMap<>();
    private Map<String, Copybook> allCopybooks = new HashMap<>();

    public SimpleASTQueryService(ASTConfig config) {
        this.config = config;
        this.loader = new ASTLoader(config.getAstBasePath().toString());
        this.cblCache = new SimpleCache<>(config.getCacheMaxSize());
        this.jclCache = new SimpleCache<>(config.getCacheMaxSize());
        this.copybookCache = new SimpleCache<>(config.getCacheMaxSize());

        logger.info("SimpleASTQueryService initialized with config: {}", config);
    }

    public SimpleASTQueryService(String astBasePath) {
        this(ASTConfig.builder().astBasePath(astBasePath).build());
    }

    public ASTConfig getConfig() {
        return config;
    }

    @Override
    public Optional<CBLFile> getCbl(String programName) {
        logger.debug("getCbl: {}", programName);

        // 1. Chercher dans le cache
        Optional<CBLFile> cached = cblCache.get("cbl:" + programName);
        if (cached.isPresent()) {
            logger.debug("Found in CBL cache: {}", programName);
            return cached;
        }

        // 2. Charger depuis le disque
        Optional<CBLFile> cbl = loader.loadCbl(programName);

        // 3. Ajouter au cache
        cbl.ifPresent(c -> {
            cblCache.put("cbl:" + programName, c);
            allCbls.put(programName, c);
        });

        return cbl;
    }

    @Override
    public Optional<JCLFile> getJcl(String jclName) {
        logger.debug("getJcl: {}", jclName);

        // 1. Chercher dans le cache
        Optional<JCLFile> cached = jclCache.get("jcl:" + jclName);
        if (cached.isPresent()) {
            logger.debug("Found in JCL cache: {}", jclName);
            return cached;
        }

        // 2. Charger depuis le disque
        Optional<JCLFile> jcl = loader.loadJcl(jclName);

        // 3. Ajouter au cache
        jcl.ifPresent(j -> {
            jclCache.put("jcl:" + jclName, j);
            allJcls.put(jclName, j);
        });

        return jcl;
    }

    @Override
    public Optional<Copybook> getCopybook(String copybookName) {
        logger.debug("getCopybook: {}", copybookName);

        // 1. Chercher dans le cache
        Optional<Copybook> cached = copybookCache.get("cpy:" + copybookName);
        if (cached.isPresent()) {
            logger.debug("Found in Copybook cache: {}", copybookName);
            return cached;
        }

        // 2. Charger depuis le disque
        Optional<Copybook> copybook = loader.loadCopybook(copybookName);

        // 3. Ajouter au cache
        copybook.ifPresent(c -> {
            copybookCache.put("cpy:" + copybookName, c);
            allCopybooks.put(copybookName, c);
        });

        return copybook;
    }

    @Override
    public Optional<Dataset> getDataset(String datasetName) {
        logger.debug("getDataset: {}", datasetName);
        return Optional.of(loader.loadDataset(datasetName).get());
    }

    @Override
    public List<JCLFile> findJclUsingCbl(String programName) {
        logger.debug("findJclUsingCbl: {}", programName);

        List<JCLFile> result = new ArrayList<>();

        // Parcourir tous les fichiers JCL disponibles
        Path basePath = config.getAstBasePath();
        try {
            Files.list(basePath)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String jclName = fileName.replace("-aggregated.json", "");

                        Optional<JCLFile> jcl = getJcl(jclName);
                        if (jcl.isPresent() && jcl.get().usesProgram(programName)) {
                            result.add(jcl.get());
                        }
                    });
        } catch (IOException e) {
            logger.error("Error scanning for JCLs using program {}: {}", programName, e.getMessage());
        }

        logger.debug("Found {} JCLs using program: {}", result.size(), programName);
        return result;
    }

    @Override
    public List<JCLFile> findJclUsingDataset(String datasetName) {
        logger.debug("findJclUsingDataset: {}", datasetName);

        List<JCLFile> result = new ArrayList<>();

        // Parcourir tous les fichiers JCL disponibles
        Path basePath = config.getAstBasePath();
        try {
            Files.list(basePath)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String jclName = fileName.replace("-aggregated.json", "");

                        Optional<JCLFile> jcl = getJcl(jclName);
                        if (jcl.isPresent() && jcl.get().usesDataset(datasetName)) {
                            result.add(jcl.get());
                        }
                    });
        } catch (IOException e) {
            logger.error("Error scanning for JCLs using dataset {}: {}", datasetName, e.getMessage());
        }

        logger.debug("Found {} JCLs using dataset: {}", result.size(), datasetName);
        return result;
    }

    @Override
    public List<CBLFile> findCblUsingCopybook(String copybookName) {
        logger.debug("findCblUsingCopybook: {}", copybookName);

        List<CBLFile> result = new ArrayList<>();

        // Parcourir tous les programmes COBOL
        Path basePath = config.getAstBasePath();
        try {
            Files.list(basePath)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String cblName = fileName.replace("-aggregated.json", "");

                        Optional<CBLFile> cbl = getCbl(cblName);
                        if (cbl.isPresent() && cbl.get().usesCopybook(copybookName)) {
                            result.add(cbl.get());
                        }
                    });
        } catch (IOException e) {
            logger.error("Error scanning for CBLs using copybook {}: {}", copybookName, e.getMessage());
        }

        logger.debug("Found {} CBLs using copybook: {}", result.size(), copybookName);
        return result;
    }

    @Override
    public List<CBLFile> findCblUsingDataset(String datasetName) {
        logger.debug("findCblUsingDataset: {}", datasetName);

        List<CBLFile> result = new ArrayList<>();

        // Parcourir tous les programmes COBOL
        Path basePath = config.getAstBasePath();
        try {
            Files.list(basePath)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String cblName = fileName.replace("-aggregated.json", "");

                        Optional<CBLFile> cbl = getCbl(cblName);
                        if (cbl.isPresent() && cbl.get().usesDataset(datasetName)) {
                            result.add(cbl.get());
                        }
                    });
        } catch (IOException e) {
            logger.error("Error scanning for CBLs using dataset {}: {}", datasetName, e.getMessage());
        }

        logger.debug("Found {} CBLs using dataset: {}", result.size(), datasetName);
        return result;
    }

    @Override
    public List<CBLFile> findCblCallees(String programName) {
        logger.debug("findCblCallees: {}", programName);

        Optional<CBLFile> program = getCbl(programName);
        if (program.isEmpty()) {
            return new ArrayList<>();
        }

        List<String> calleeNames = program.get().getCallees();
        if (calleeNames == null || calleeNames.isEmpty()) {
            return new ArrayList<>();
        }

        // Charger tous les programs appelés
        List<CBLFile> callees = new ArrayList<>();
        for (String calleeName : calleeNames) {
            Optional<CBLFile> callee = getCbl(calleeName);
            if (callee.isPresent()) {
                callees.add(callee.get());
            }
        }

        logger.debug("Found {} callees for program: {}", callees.size(), programName);
        return callees;
    }

    @Override
    public List<CBLFile> findCblCallers(String programName) {
        logger.debug("findCblCallers: {}", programName);

        Optional<CBLFile> program = getCbl(programName);
        if (program.isEmpty()) {
            return new ArrayList<>();
        }

        List<String> callerNames = program.get().getCallers();
        if (callerNames == null || callerNames.isEmpty()) {
            return new ArrayList<>();
        }

        // Charger tous les programs appelants
        List<CBLFile> callers = new ArrayList<>();
        for (String callerName : callerNames) {
            Optional<CBLFile> caller = getCbl(callerName);
            if (caller.isPresent()) {
                callers.add(caller.get());
            }
        }

        logger.debug("Found {} callers for program: {}", callers.size(), programName);
        return callers;
    }

    @Override
    public List<Copybook> findCopybooksUsedByCbl(String programName) {
        logger.debug("findCopybooksUsedByCbl: {}", programName);

        Optional<CBLFile> program = getCbl(programName);
        if (program.isEmpty()) {
            return new ArrayList<>();
        }

        List<String> copybooks = program.get().getCopybooks();
        if (copybooks == null || copybooks.isEmpty()) {
            return new ArrayList<>();
        }

        // Charger tous les copybooks utilisés
        List<Copybook> result = new ArrayList<>();
        for (String cpyName : copybooks) {
            Optional<Copybook> cpy = getCopybook(cpyName);
            if (cpy.isPresent()) {
                result.add(cpy.get());
            }
        }

        logger.debug("Found {} copybooks used by program: {}", result.size(), programName);
        return result;
    }

    @Override
    public List<Copybook> findCopybooksUsedByCopybook(String copybookName) {
        logger.debug("findCopybooksUsedByCopybook: {}", copybookName);

        Optional<Copybook> copybook = getCopybook(copybookName);
        if (copybook.isEmpty()) {
            return new ArrayList<>();
        }

        List<String> includes = copybook.get().getIncludes();
        if (includes == null || includes.isEmpty()) {
            return new ArrayList<>();
        }

        // Charger les copybooks avec cycle detection
        Map<String, Copybook> allCopybooksMap = new HashMap<>(allCopybooks);

        // Ajouter le copybook courant s'il n'existe pas
        if (!allCopybooksMap.containsKey(copybookName)) {
            allCopybooksMap.put(copybookName, copybook.get());
        }

        // Charger les includes manquants
        for (String include : includes) {
            if (!allCopybooksMap.containsKey(include)) {
                Optional<Copybook> cpy = getCopybook(include);
                cpy.ifPresent(c -> allCopybooksMap.put(include, c));
            }
        }

        // Utiliser le CycleDetector pour récupérer les includes sans cycles
        List<Copybook> result = CycleDetector.getIncludesWithoutCycles(
                copybookName, allCopybooksMap);

        // Vérifier et logger les cycles
        if (config.isCycleDetectionEnabled()) {
            Optional<List<String>> cycle = CycleDetector.findCyclePath(copybookName, allCopybooksMap);
            if (cycle.isPresent()) {
                logger.warn("Cycle detected in copybook dependencies: {}",
                        CycleDetector.formatCyclePath(cycle.get()));
            }
        }

        logger.debug("Found {} copybooks used by copybook: {}", result.size(), copybookName);
        return result;
    }

    // ==================== Utilitaires ====================

    /**
     * Retourne les copybooks utilisés par un programme avec includes résolus
     * (Peuplés par preloadAll() lors du startup)
     */
    public List<Copybook> findCopybooksWithResolvedIncludes(String programName) {
        logger.debug("findCopybooksWithResolvedIncludes: {}", programName);

        Optional<CBLFile> program = getCbl(programName);
        if (program.isEmpty()) {
            return new ArrayList<>();
        }

        // Retourner copybooksList peuplée par ASTParser
        CBLFile cbl = program.get();
        List<Copybook> copybooksList = cbl.getCopybooksList();

        if (copybooksList == null || copybooksList.isEmpty()) {
            logger.debug("No copybooks found in copybooksList for: {}", programName);
            return new ArrayList<>();
        }

        logger.debug("Found {} copybooks with resolved includes for: {}", 
            copybooksList.size(), programName);
        return copybooksList;
    }

    /**
     * Retourne TOUS les includes résolus d'un copybook (récursif)
     * Les includes résolus sont peuplés par preloadAll()
     */
    public List<Copybook> findAllIncludesRecursive(String copybookName) {
        logger.debug("findAllIncludesRecursive: {}", copybookName);

        Optional<Copybook> cpy = getCopybook(copybookName);
        if (cpy.isEmpty()) {
            return new ArrayList<>();
        }

        Copybook copybook = cpy.get();
        List<Copybook> resolved = copybook.getResolvedIncludes();

        if (resolved == null || resolved.isEmpty()) {
            logger.debug("No resolved includes found for copybook: {}", copybookName);
            return new ArrayList<>();
        }

        logger.debug("Found {} resolved includes for: {}", resolved.size(), copybookName);
        return resolved;
    }

    /**
     * Précharge tous les ASTs disponibles et résout les includes
     * À appeler une seule fois au démarrage de l'application
     */
    public void preloadAllAndResolveIncludes() {
        logger.info("Preloading all ASTs and resolving includes...");
        Path basePath = config.getAstBasePath();
        
        try {
            // Vérifier si le répertoire report existe
            Path reportPath = basePath.resolve("report");
            Path scanPath = Files.exists(reportPath) ? reportPath : basePath;
            
            logger.info("Scanning for AST files in: {}", scanPath.toAbsolutePath());
            
            // Scanner récursivement pour trouver tous les fichiers *-aggregated.json
            Files.walk(scanPath, 10)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String name = fileName.replace("-aggregated.json", "");
                        logger.debug("Loading program: {}", name);
                        getCbl(name);  // Load and cache
                    });

            // Résoudre les includes pour tous les copybooks
            CopybookIncludesResolver resolver = new CopybookIncludesResolver();
            for (Copybook copybook : allCopybooks.values()) {
                try {
                    resolver.populateResolvedIncludes(copybook, this);
                } catch (Exception e) {
                    logger.warn("Error resolving includes for copybook {}: {}",
                        copybook.getName(), e.getMessage());
                }
            }

            logger.info("Preload complete. Cached {} CBLs, {} Copybooks. Includes resolved.",
                allCbls.size(), allCopybooks.size());
            
            // Construire le graphe des callers après le chargement de tous les programmes
            buildCallGraph();
            
        } catch (IOException e) {
            logger.error("Error during preload: {}", e.getMessage(), e);
        }
    }

    /**
     * Construit le graphe d'appels en calculant les callers pour tous les programmes.
     * Cette méthode doit être appelée après le chargement de tous les programmes.
     * 
     * Algorithme:
     * 1. Parcourir tous les programmes et leurs callees
     * 2. Construire une map inverse: callee -> list of callers
     * 3. Mettre à jour chaque programme avec ses callers
     */
    public void buildCallGraph() {
        logger.info("Building call graph...");
        
        // 1. Construire la map inverse (callee -> callers)
        Map<String, List<String>> callersMap = new HashMap<>();
        
        for (CBLFile program : allCbls.values()) {
            String programName = program.getName();
            List<String> callees = program.getCallees();
            
            if (callees != null && !callees.isEmpty()) {
                for (String callee : callees) {
                    callersMap.computeIfAbsent(callee, k -> new ArrayList<>()).add(programName);
                }
            }
        }
        
        // 2. Mettre à jour tous les programmes avec leurs callers
        int updatedCount = 0;
        for (CBLFile program : allCbls.values()) {
            String programName = program.getName();
            List<String> callers = callersMap.get(programName);
            
            if (callers != null && !callers.isEmpty()) {
                // Trier pour cohérence
                Collections.sort(callers);
                program.setCallers(callers);
                updatedCount++;
                
                logger.debug("Program {} has {} callers: {}", programName, callers.size(), callers);
            }
        }
        
        logger.info("Call graph built. Updated {} programs with callers", updatedCount);
        
        // Log statistiques
        int programsWithCallees = (int) allCbls.values().stream()
                .filter(p -> p.getCallees() != null && !p.getCallees().isEmpty())
                .count();
        int programsWithCallers = (int) allCbls.values().stream()
                .filter(p -> p.getCallers() != null && !p.getCallers().isEmpty())
                .count();
        
        logger.info("Call graph stats: {} programs have callees, {} programs have callers",
                programsWithCallees, programsWithCallers);
    }

    /**
     * Retourne les statistiques du cache
     */
    public String getCacheStats() {
        return String.format("CBL Cache: %s, JCL Cache: %s, Copybook Cache: %s",
                cblCache.getStats(), jclCache.getStats(), copybookCache.getStats());
    }

    /**
     * Vide tous les caches
     */
    public void clearCaches() {
        cblCache.clear();
        jclCache.clear();
        copybookCache.clear();
        allCbls.clear();
        allJcls.clear();
        allCopybooks.clear();
        logger.info("All caches cleared");
    }

    /**
     * Retourne le nombre de CBLs en cache
     */
    public int getCblCacheSize() {
        return cblCache.size();
    }

    /**
     * Retourne le nombre de JCLs en cache
     */
    public int getJclCacheSize() {
        return jclCache.size();
    }

    /**
     * Retourne le nombre de Copybooks en cache
     */
    public int getCopybookCacheSize() {
        return copybookCache.size();
    }

    @Override
    public List<CBLFile> getAllCbl() {
        logger.info("getAllCbl called - scanning for CBL files...");
        
        // Charger tous les fichiers .cbl avec scan récursif dans report/
        List<CBLFile> allPrograms = new ArrayList<>();
        Path basePath = config.getAstBasePath();
        logger.info("Base path: {}", basePath.toAbsolutePath());
        
        try {
            // Scanner récursivement dans report/*/ast/aggregated/
            Path reportPath = basePath.resolve("report");
            logger.info("Looking for report directory at: {}", reportPath.toAbsolutePath());
            
            if (Files.exists(reportPath) && Files.isDirectory(reportPath)) {
                logger.info("Report directory exists, scanning recursively...");
                Files.walk(reportPath)
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        boolean matches = p.getFileName().toString().endsWith("-aggregated.json");
                        if (matches) {
                            logger.debug("Found aggregated file: {}", p);
                        }
                        return matches;
                    })
                    .filter(p -> {
                        boolean contains = p.toString().contains("ast" + java.io.File.separator + "aggregated");
                        if (!contains) {
                            logger.debug("Skipping file (not in ast/aggregated): {}", p);
                        }
                        return contains;
                    })
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String programName = fileName.replace("-aggregated.json", "");
                        logger.info("Processing AST file: {} for program: {}", p, programName);
                        Optional<CBLFile> cbl = getCbl(programName);
                        if (cbl.isPresent()) {
                            allPrograms.add(cbl.get());
                            logger.info("Successfully loaded program: {}", programName);
                        } else {
                            logger.warn("Failed to load program: {}", programName);
                        }
                    });
            } else {
                logger.warn("Report directory not found at: {}, trying fallback...", reportPath.toAbsolutePath());
                // Fallback: scanner au niveau racine (ancien format)
                Files.list(basePath)
                    .filter(p -> p.getFileName().toString().endsWith(".cbl-aggregated.json"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String programName = fileName.replace(".cbl-aggregated.json", "");
                        getCbl(programName).ifPresent(allPrograms::add);
                    });
            }
        } catch (IOException e) {
            logger.error("Error listing all CBL files: {}", e.getMessage(), e);
        }
        
        logger.info("Completed scan - Found {} CBL programs", allPrograms.size());
        
        // NOUVEAU: Créer index inversé JCL→Programme
        buildJclProgramIndex(allPrograms);
        
        return allPrograms;
    }
    
    /**
     * Crée un index inversé pour lier chaque programme aux JCL qui l'appellent
     */
    private void buildJclProgramIndex(List<CBLFile> programs) {
        logger.debug("Building JCL→Program index...");
        
        // 1. Charger tous les JCL si pas encore fait
        List<JCLFile> allJcls = getAllJcl();
        
        // 2. Pour chaque programme, trouver les JCL qui le référencent
        for (CBLFile program : programs) {
            List<String> callingJcls = new ArrayList<>();
            
            for (JCLFile jcl : allJcls) {
                if (jcl.getPrograms() != null && jcl.getPrograms().contains(program.getName())) {
                    callingJcls.add(jcl.getName());
                }
            }
            
            // 3. Mettre à jour le programme avec ses JCL appelants
            program.setJcls(callingJcls);
            logger.debug("Program {} is called by {} JCL(s): {}", 
                program.getName(), callingJcls.size(), callingJcls);
        }
        
        logger.info("JCL→Program index built: {} programs indexed", programs.size());
    }

    @Override
    public List<JCLFile> getAllJcl() {
        logger.debug("getAllJcl called");
        
        Path basePath = config.getAstBasePath();
        Path jclAnalysisPath = basePath.resolve("jcl-analysis.json");
        
        // 1. Charger depuis jcl-analysis.json en priorité
        if (Files.exists(jclAnalysisPath)) {
            logger.info("Loading JCLs from jcl-analysis.json");
            JclAnalysisParser parser = new JclAnalysisParser();
            List<JCLFile> jclFiles = parser.parseJclAnalysis(jclAnalysisPath);
            logger.info("Loaded {} JCLs from jcl-analysis.json", jclFiles.size());
            return jclFiles;
        }
        
        // 2. Fallback: scanner les fichiers AST
        logger.info("jcl-analysis.json not found, falling back to AST scanning");
        
        return scanAstForJclFiles();
    }

    /**
     * Scanner les fichiers AST pour extraire les JCL
     */
    private List<JCLFile> scanAstForJclFiles() {
        List<JCLFile> jclFiles = new ArrayList<>();
        Path basePath = config.getAstBasePath();
        
        try {
            Path reportPath = basePath.resolve("report");
            
            if (Files.exists(reportPath) && Files.isDirectory(reportPath)) {
                // Scanner récursivement dans report/*/ast/aggregated/
                Files.walk(reportPath)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .filter(p -> p.toString().contains("ast" + java.io.File.separator + "aggregated"))
                    .forEach(p -> {
                        String jclName = extractNameFromAggregatedFile(p);
                        getJcl(jclName).ifPresent(jclFiles::add);
                    });
            } else {
                // Fallback: scanner au niveau racine
                Files.list(basePath)
                    .filter(p -> p.getFileName().toString().endsWith("-aggregated.json"))
                    .filter(p -> !p.getFileName().toString().endsWith(".cbl-aggregated.json"))
                    .forEach(p -> {
                        String jclName = extractNameFromAggregatedFile(p);
                        getJcl(jclName).ifPresent(jclFiles::add);
                    });
            }
        } catch (IOException e) {
            logger.error("Error scanning AST for JCL files: {}", e.getMessage());
        }
        
        logger.info("Found {} JCL files from AST scanning", jclFiles.size());
        return jclFiles;
    }

    /**
     * Extrait le nom depuis un fichier aggregated (ex: CBEXPORT-aggregated.json → CBEXPORT)
     */
    private String extractNameFromAggregatedFile(Path filePath) {
        return filePath.getFileName().toString().replace("-aggregated.json", "");
    }

    @Override
    public List<Copybook> getAllCopybooks() {
        logger.debug("getAllCopybooks called");
        
        // Map pour accumuler les usages de chaque copybook
        Map<String, List<String>> copybookUsages = new HashMap<>();
        
        // 1. Scanner tous les programmes pour collecter les copybooks
        List<CBLFile> allPrograms = getAllCbl();
        
        for (CBLFile program : allPrograms) {
            if (program.getCopybooks() != null) {
                for (String copybookName : program.getCopybooks()) {
                    copybookUsages.computeIfAbsent(copybookName, k -> new ArrayList<>())
                        .add(program.getName());
                }
            }
        }
        
        // 2. Créer les objets Copybook avec toutes les données
        List<Copybook> result = new ArrayList<>();
        for (Map.Entry<String, List<String>> entry : copybookUsages.entrySet()) {
            Copybook copybook = Copybook.builder()
                .name(entry.getKey())
                .path("")
                .parseStatus(ParseStatus.SUCCESS)
                .lastModified(System.currentTimeMillis())
                .usedByCobol(entry.getValue())
                .usedByCopybook(new ArrayList<>())
                .includes(new ArrayList<>())
                .build();
            result.add(copybook);
        }
        
        logger.info("Found {} unique copybooks across {} programs", result.size(), allPrograms.size());
        return result;
    }

    @Override
    public List<Dataset> getAllDatasets() {
        logger.debug("getAllDatasets called");
        
        // Maps pour accumuler les usages de chaque dataset
        Map<String, List<String>> datasetUsagesByJcl = new HashMap<>();
        Map<String, List<String>> datasetUsagesByCobol = new HashMap<>();
        
        // 1. Collecter datasets depuis JCL
        List<JCLFile> allJcls = getAllJcl();
        for (JCLFile jcl : allJcls) {
            if (jcl.getDatasets() != null) {
                for (String datasetName : jcl.getDatasets()) {
                    datasetUsagesByJcl.computeIfAbsent(datasetName, k -> new ArrayList<>())
                        .add(jcl.getName());
                }
            }
        }
        
        // 2. Collecter datasets depuis CBL
        List<CBLFile> allPrograms = getAllCbl();
        for (CBLFile program : allPrograms) {
            if (program.getDatasets() != null) {
                for (String datasetName : program.getDatasets()) {
                    datasetUsagesByCobol.computeIfAbsent(datasetName, k -> new ArrayList<>())
                        .add(program.getName());
                }
            }
        }
        
        // 3. Créer les objets Dataset avec toutes les données
        Set<String> allDatasetNames = new HashSet<>();
        allDatasetNames.addAll(datasetUsagesByJcl.keySet());
        allDatasetNames.addAll(datasetUsagesByCobol.keySet());
        
        List<Dataset> result = new ArrayList<>();
        for (String datasetName : allDatasetNames) {
            Dataset dataset = Dataset.builder()
                .name(datasetName)
                .path("")
                .parseStatus(ParseStatus.SUCCESS)
                .lastModified(System.currentTimeMillis())
                .usedByJcl(datasetUsagesByJcl.getOrDefault(datasetName, new ArrayList<>()))
                .usedByCobol(datasetUsagesByCobol.getOrDefault(datasetName, new ArrayList<>()))
                .build();
            result.add(dataset);
        }
        
        logger.info("Found {} unique datasets across {} JCLs and {} programs", 
            result.size(), allJcls.size(), allPrograms.size());
        return result;
    }
}

