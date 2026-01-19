# SMOJOL REST API

API REST exposant les données AST COBOL avec graphe d'appels complet (callees/callers).

**Version**: 1.0.0  
**Framework**: Javalin 5.6.3  
**Port**: 8080 (défaut)  
**Status**: ✅ Production

---

## 🚀 Quick Start

```bash
# 1. Compiler l'API
mvn clean package -DskipTests

# 2. Générer les AST (requis une fois)
bash scripts/scan_and_analyze_project.sh /path/to/cobol/source ./out

# 3. Démarrer l'API
cd smojol-rest-api
java -Dast.base.path=../out -jar target/smojol-rest-api-1.0.0.jar

# Logs attendus:
# ✅ Scanning for AST files in: .../out/report
# ✅ Preload complete. Cached 39 CBLs
# ✅ Call graph built. Updated 4 programs with callers
# ✅ SmojolRestAPI started on port 8080

# 4. Tester
curl http://localhost:8080/api/programs/CBACT01C
```

---

## 📡 Endpoints

```bash
# Health
GET /api/health

# Programmes COBOL
GET /api/programs
GET /api/programs/{name}

# Fichiers JCL
GET /api/jcls
GET /api/jcls/{name}

# Copybooks
GET /api/copybooks
GET /api/copybooks/{name}

# Datasets
GET /api/datasets
GET /api/datasets/{name}
```

---

## 🔄 Recharger les Données

Après régénération des AST, **redémarrer l'API** :

```bash
# 1. Régénérer
bash scripts/scan_and_analyze_project.sh /new/source ./out

# 2. Redémarrer (Ctrl+C puis relancer)
java -Dast.base.path=../out -jar target/smojol-rest-api-1.0.0.jar
```

💡 L'API charge les données au démarrage uniquement.

---

## 🔧 Configuration

```bash
# Port personnalisé
java -Dserver.port=8081 -jar target/smojol-rest-api-1.0.0.jar

# Chemin AST absolu
java -Dast.base.path=/full/path/to/out -jar target/smojol-rest-api-1.0.0.jar

# Logs debug
java -Dorg.slf4j.simpleLogger.defaultLogLevel=DEBUG -jar target/smojol-rest-api-1.0.0.jar
```

---

## 📦 Exemples de Réponses

### Programme COBOL avec callees
```json
{
  "name": "CBACT01C",
  "path": "",
  "copybooks": ["CVACT01Y", "CODATECN"],
  "jcls": ["READACCT"],
  "callees": ["CEE3ABD", "COBDATFT"],
  "callers": []
}
```

### Programme très appelé (callers)
```json
{
  "name": "CEE3ABD",
  "callees": [],
  "callers": [
    "CBACT01C", "CBACT02C", "CBACT03C", "CBACT04C",
    "CBACT05C", "CBACT09C", "CBACT15C", "CBLIS01C",
    "CBLIS02C", "CBLIS03C"
  ]
}
```

### Copybook très utilisé
```json
{
  "name": "CVCUS01Y",
  "usedBy": [
    "CBCUS01C", "CBEXPORT", "CBIMPORT", "CBTRN01C",
    "COACTUPC", "COACTVWC", "COCRDSLC", "COCRDUPC",
    "COPAUA0C", "COPAUS0C"
  ]
}
```

### JCL
```json
{
  "name": "CBIMPORT",
  "jobName": "CBIMPORT",
  "programs": ["CBIMPORT"],
  "datasets": ["CUSTFILE"]
}
```

---

## 🛠️ Architecture

```
scan_and_analyze_project.sh
         │
         ▼
┌────────────────────┐
│  out/              │
│  ├─ jcl-analysis   │  46 JCLs
│  └─ report/        │  ASTs programmes
└─────────┬──────────┘
          │
          ▼
┌─────────────────────┐
│  SmojolRestAPI      │
│  └─ Javalin (8080)  │
└─────────┬───────────┘
          │
          ▼ HTTP/JSON
┌─────────────────────┐
│  smojol-ui (3000)   │
└─────────────────────┘
```

---

## 🐛 Troubleshooting

**Port occupé ?**
```bash
# Windows
netstat -ano | findstr :8080
taskkill /F /PID <PID>

# Linux/Mac
lsof -ti:8080 | xargs kill -9
```

**AST non trouvés ?**
- Vérifier: `ls -la ./out/report`
- Vérifier: `ls -la ./out/jcl-analysis.json`

**UI ne charge pas ?**
- API tourne ? `curl http://localhost:8080/api/health`
- Console navigateur (F12) pour voir erreurs

---

**Dernière mise à jour**: Janvier 2026
