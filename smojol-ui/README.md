# SMOJOL UI

Interface web pour explorer programmes COBOL, JCL, copybooks et datasets.

**Version**: 1.0.0  
**Stack**: ETA Templates + Vanilla JS + REST API

---

## 🚀 Quick Start

```bash
# 1. Installer (première fois)
npm install

# 2. Builder
npm run build

# 3. Démarrer l'API (autre terminal)
cd ../smojol-rest-api
java -Dast.base.path=../out -jar target/smojol-rest-api-1.0.0.jar

# 4. Servir l'UI
cd ../smojol-ui
python -m http.server 3000

# 5. Ouvrir
# http://localhost:3000
```

---

## 📋 Fonctionnalités

| Onglet | Description |
|--------|-------------|
| 📄 **COBOL** | Programmes avec JCLs appelants, copybooks |
| ⚙️ **JCL** | Fichiers JCL avec programmes, datasets |
| 📋 **Copybooks** | Copybooks avec programmes utilisateurs |
| 💾 **Datasets** | Datasets avec JCLs et programmes |

**Recherche** : Barre globale en temps réel (300ms debounce)  
**Navigation** : Clic sur carte → Panneau détails  
**Interface** : Dark theme moderne, responsive

---

## 🏗️ Architecture

```
Templates ETA
    ↓
npm run build
    ↓
index-generated.html
    ↓ HTTP
smojol-rest-api (8080)
    ↓ JSON
Données affichées
```

---

## 🔧 Configuration

**Port UI** : `3000` (défaut)
```bash
python -m http.server 8090  # Port custom
```

**API Endpoint** : `http://localhost:8080/api`  
Pour changer, modifier `templates/scripts/data-loader.eta` et rebuild.

---

## 📦 Scripts

```bash
npm run build    # Compile ETA → HTML
npm run watch    # Recompile automatique
```

---

## 🔄 Workflow

1. Générer AST : `bash scripts/scan_and_analyze_project.sh /source ../out`
2. Démarrer API : `java -Dast.base.path=../out -jar target/smojol-rest-api-1.0.0.jar`
3. Servir UI : `python -m http.server 3000`
4. Naviguer : `http://localhost:3000`

---

## 🐛 Troubleshooting

**UI ne charge pas ?**
- API tourne sur 8080 ? `curl http://localhost:8080/api/health`
- Console navigateur (F12) pour erreurs

**Erreur CORS ?**
- API a CORS activé pour tous domaines

**Build échoue ?**
```bash
rm -rf node_modules
npm install
npm run build
```

---

## 📁 Structure

```
smojol-ui/
├── templates/           # Templates ETA
│   ├── components/     # Composants réutilisables
│   └── scripts/        # JavaScript modulaire
├── build.js            # Script compilation
├── config.js           # Configuration build
└── index-generated.html # UI compilée (généré)
```

---

## 🎯 Technologies

- **ETA Templates** - Moteur template compact
- **Vanilla JS** - Léger, pas de framework
- **REST API** - Communication via fetch()
- **Python HTTP** - Serveur fichiers statiques

---

**Dernière mise à jour**: Janvier 2026
