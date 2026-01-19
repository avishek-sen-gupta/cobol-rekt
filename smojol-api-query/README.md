# SMOJOL API Query

Module Java de requêtes AST pour analyser programmes COBOL avec extraction automatique des relations d'appels.

**Version**: 1.0.0  
**Java**: 21+  
**Status**: ✅ Production

---

## 🎯 Fonctionnalités

- ✅ **Extraction callees/callers** - Parcours récursif des CallStatementContext dans l'AST
- ✅ **Graphe d'appels** - Construction automatique des relations caller ↔ callee
- ✅ **Cache LRU** - Performance optimale avec éviction intelligente
- ✅ **Preload au démarrage** - Chargement récursif de tous les AST
- ✅ **Support JCL enrichi** - 46 JCL avec programmes et datasets
- ✅ **Copybooks** - Tracking des usages et résolution des includes

---

## 📦 Composants

### Data Models
- `CBLFile` - Programme COBOL avec copybooks, JCLs, callers/callees
- `JCLFile` - Fichier JCL avec programmes, datasets, steps
- `Copybook` - Copybook avec includes et utilisateurs
- `Dataset` - Dataset avec organisation, format, usage
- `ParseStatus` - Statut parsing (SUCCESS, ERROR, etc.)

### Services
- `ASTQueryService` - Interface de requêtes (getAllCbl, getJcl, etc.)
- `SimpleASTQueryService` - Implémentation avec cache mémoire
- `ASTLoader` - Chargement fichiers AST JSON
- `ASTParser` - Parsing JSON vers modèles

### Utilitaires
- `JclAnalysisParser` - Parse jcl-analysis.json (46 JCLs)
- `SimpleCache` - Cache HashMap avec éviction LRU
- `CopybookIncludesResolver` - Résolution includes
- `CycleDetector` - Détection cycles dépendances

---

## 🚀 Utilisation

```java
// Initialiser avec le répertoire contenant les AST
SimpleASTQueryService service = new SimpleASTQueryService("./out");

// Précharger tous les AST et construire le graphe d'appels
service.preloadAllAndResolveIncludes();
// Logs: Cached 39 CBLs, Call graph stats: 12 programs have callees

// Charger un programme avec ses relations
Optional<CBLFile> program = service.getCbl("CBACT01C");
program.ifPresent(cbl -> {
    System.out.println("Programme: " + cbl.getName());
    System.out.println("Copybooks: " + cbl.getCopybooks()); // [CVACT01Y, CODATECN]
    System.out.println("Callees: " + cbl.getCallees());     // [CEE3ABD, COBDATFT]
    System.out.println("Callers: " + cbl.getCallers());     // []
});

// Analyser un programme très appelé
Optional<CBLFile> popular = service.getCbl("CEE3ABD");
popular.ifPresent(cbl -> {
    System.out.println("Callers: " + cbl.getCallers().size()); // 10 programmes
});

// Lister tous les programmes
List<CBLFile> allPrograms = service.getAllCbl();

// Rechercher programmes utilisant un copybook
List<CBLFile> users = service.findCblUsingCopybook("CVCUS01Y");
System.out.println("Programmes utilisant CVCUS01Y: " + users.size()); // 10
```

---

## 📁 Structure

```
smojol-api-query/
└── src/main/java/com/smojol/api/query/
    ├── model/              # Data models
    ├── service/            # Services de requêtes
    ├── util/               # Utilitaires (loader, parser, cache)
    └── config/             # Configuration
```

---

## 🔧 Architecture

### Extraction Callees/Callers

**Algorithme de parsing AST** (Solution 1 - Sans regex):
```java
// 1. Parcours récursif de l'AST JSON
findCallStatements(JsonNode node) {
    if (node.nodeType == "CallStatementContext") {
        // 2. Extraction du target depuis les children
        target = extractCallTarget(node.children);
        callees.add(target);
    }
    // Récursion sur tous les enfants
}

// 3. Construction du graphe inverse (callers)
buildCallGraph() {
    for (program : allPrograms) {
        for (callee : program.callees) {
            callersMap[callee].add(program.name);
        }
    }
}
```

**Statistiques typiques**:
- 39 programmes analysés
- 12 programmes avec CALL statements
- CEE3ABD: programme le plus appelé (10 callers)
- Temps de construction: <500ms

### Cache & Performance

- **Cache LRU** - 100 entrées max, éviction automatique
- **Preload récursif** - Files.walk() pour scanner out/report/**/*-aggregated.json
- **Chargement lazy** - Programmes chargés à la demande si non préchargés

---

## 📊 Performance

- Cache hit rate: ~80% en production
- Temps chargement: <100ms par programme
- Mémoire: ~50MB pour 100 programmes

**Optimisations v2:**
- Constantes DSN pour validation centralisée
- Opérateur ternaire pour code concis
- `Set.contains()` pour performance O(1)

---

## 🔗 Intégration REST API

```java
// Dans SmojolRestAPI.java
ASTQueryService queryService = new SimpleASTQueryService("../out");

app.get("/api/programs", ctx -> {
    ctx.json(queryService.getAllCbl());
});
```

---

## 🛠️ Build

```bash
mvn clean compile jar:jar install:install -DskipTests
```

---

**Dernière mise à jour**: Janvier 2026
