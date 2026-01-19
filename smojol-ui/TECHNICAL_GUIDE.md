# Guide Technique SMOJOL UI

Documentation technique complète pour comprendre, maintenir et modifier l'interface web SMOJOL.

**Version**: 1.0.0  
**Stack**: ETA Templates + Vanilla JavaScript + REST API  
**Dernière mise à jour**: 2026-01-19

---

## 📋 Table des Matières

1. [Vue d'ensemble](#vue-densemble)
2. [Architecture](#architecture)
3. [Structure des fichiers](#structure-des-fichiers)
4. [Système de templates ETA](#système-de-templates-eta)
5. [Composants JavaScript](#composants-javascript)
6. [Styles et thème](#styles-et-thème)
7. [Communication API](#communication-api)
8. [Modifier l'UI](#modifier-lui)
9. [Ajouter de nouvelles fonctionnalités](#ajouter-de-nouvelles-fonctionnalités)
10. [Debugging](#debugging)

---

## 🎯 Vue d'ensemble

### Concept

SMOJOL UI est une **Single Page Application (SPA)** générée statiquement via des templates ETA. L'application communique avec l'API REST pour afficher les données COBOL.

### Stack technique

| Composant | Technologie | Raison |
|-----------|-------------|---------|
| **Templates** | ETA (Embedded JavaScript) | Léger, rapide, syntaxe simple |
| **JavaScript** | Vanilla ES6+ | Pas de framework lourd, performance |
| **CSS** | CSS3 natif | Variables CSS, flexbox, grid |
| **Build** | Node.js | Génération HTML statique |
| **Serveur** | Python http.server | Simple, aucune dépendance |
| **API** | REST JSON | Communication avec Java backend |

### Workflow de développement

```
┌─────────────┐
│  Templates  │  templates/*.eta
│     ETA     │  
└──────┬──────┘
       │ npm run build
       ↓
┌─────────────┐
│  Build.js   │  Compile templates → HTML
└──────┬──────┘
       │ génère
       ↓
┌─────────────┐
│    HTML     │  index-generated.html
│   Statique  │  (tout en un fichier)
└──────┬──────┘
       │ python -m http.server
       ↓
┌─────────────┐
│  Navigateur │  http://localhost:3000
└──────┬──────┘
       │ fetch()
       ↓
┌─────────────┐
│  REST API   │  http://localhost:8080/api
│   (Java)    │
└─────────────┘
```

---

## 🏗️ Architecture

### Philosophie

**Simplicité > Complexité**
- Pas de framework JS (React, Vue, Angular)
- Pas de build system complexe (Webpack, Vite)
- Tout en un seul fichier HTML généré
- Vanilla JavaScript moderne (ES6+)

### Pattern architectural

**Template-based SPA with Client-side Rendering**

```
Templates (server)  →  HTML statique  →  JavaScript (client)  →  API (server)
                           ↓
                    Tout le code inline
                    (CSS + JS dans HTML)
```

**Avantages**:
- ✅ Un seul fichier à déployer
- ✅ Pas de CORS avec l'API
- ✅ Chargement rapide
- ✅ Debugging facile
- ✅ Aucune dépendance runtime

---

## 📁 Structure des fichiers

```
smojol-ui/
├── build.js                    # Script de build ETA → HTML
├── config.js                   # Configuration (API URL, etc.)
├── package.json                # Dépendances npm
│
├── templates/                  # Templates ETA
│   ├── index.eta              # Template principal
│   ├── layout.eta             # Structure HTML de base
│   ├── styles.eta             # Styles CSS
│   │
│   ├── components/            # Composants réutilisables
│   │   ├── header.eta        # En-tête avec onglets
│   │   ├── search-bar.eta    # Barre de recherche
│   │   ├── cards.eta         # Cartes pour listes
│   │   └── details.eta       # Panneau détails
│   │
│   └── scripts/              # Scripts JavaScript
│       ├── app.eta           # Application principale
│       ├── data-loader.eta   # Chargement API
│       ├── ui-manager.eta    # Gestion UI (tabs, search)
│       └── state.eta         # Gestion état global
│
├── index-generated.html       # 📄 HTML final généré (déployable)
├── index.html                 # HTML manuel (fallback)
│
└── README.md                  # Documentation utilisateur
```

### Fichiers clés

| Fichier | Rôle | Quand modifier |
|---------|------|----------------|
| `build.js` | Compile templates | Changer API URL, config |
| `templates/index.eta` | Point d'entrée | Nouvelle structure page |
| `templates/scripts/app.eta` | Logique principale | Nouvelle fonctionnalité |
| `templates/scripts/data-loader.eta` | Communication API | Nouveaux endpoints |
| `templates/components/*.eta` | UI composants | Modifier design |
| `templates/styles.eta` | CSS global | Changer styles, thème |

---

## 🎨 Système de templates ETA

### Qu'est-ce qu'ETA ?

**ETA** (Embedded Templates, Async) est un moteur de template JavaScript léger et rapide.

**Syntaxe de base**:
```html
<!-- Variables -->
<%= variable %>          <!-- Escaped -->
<%~ variable %>          <!-- Raw HTML -->

<!-- Logique -->
<% if (condition) { %>
  <p>Content</p>
<% } %>

<!-- Boucles -->
<% data.forEach(item => { %>
  <div><%= item.name %></div>
<% }) %>

<!-- Includes -->
<%~ include('components/header') %>
```

### Structure d'un template

**Exemple: `templates/components/search-bar.eta`**
```html
<!-- Barre de recherche globale -->
<div class="search-container">
    <input 
        type="text" 
        id="globalSearch" 
        placeholder="🔍 Rechercher..."
        autocomplete="off"
    />
    <div id="searchResults" class="search-results"></div>
</div>

<style>
.search-container {
    position: relative;
    max-width: 600px;
    margin: 20px auto;
}

#globalSearch {
    width: 100%;
    padding: 12px 20px;
    border: 2px solid var(--border-color);
    border-radius: 8px;
    font-size: 16px;
    background: var(--card-bg);
    color: var(--text-color);
}
</style>
```

### Passage de données

**Dans `templates/index.eta`**:
```html
<%~ include('components/header', { 
    tabs: ['COBOL', 'JCL', 'Copybooks', 'Datasets'],
    activeTab: 'COBOL'
}) %>
```

**Dans `templates/components/header.eta`**:
```html
<nav class="tabs">
    <% it.tabs.forEach(tab => { %>
        <button 
            class="tab <%= it.activeTab === tab ? 'active' : '' %>"
            data-tab="<%= tab %>"
        >
            <%= tab %>
        </button>
    <% }) %>
</nav>
```

### Génération du HTML final

**Commande**: `npm run build`

**Process**:
1. `build.js` lit `templates/index.eta`
2. ETA résout tous les `include()`
3. Variables remplacées (apiUrl, etc.)
4. Output: `index-generated.html` (fichier unique)

---

## ⚙️ Composants JavaScript

### Architecture JS

**Pattern Module**: Chaque module expose des fonctions publiques

```javascript
// templates/scripts/state.eta
const State = {
    currentTab: 'COBOL',
    searchQuery: '',
    selectedItem: null,
    
    setTab(tab) {
        this.currentTab = tab;
        this.emit('tabChanged', tab);
    },
    
    // Event emitter pattern
    listeners: {},
    on(event, callback) {
        if (!this.listeners[event]) {
            this.listeners[event] = [];
        }
        this.listeners[event].push(callback);
    },
    emit(event, data) {
        if (this.listeners[event]) {
            this.listeners[event].forEach(cb => cb(data));
        }
    }
};
```

### Modules principaux

#### 1. **State** (`templates/scripts/state.eta`)

Gestion de l'état global de l'application.

```javascript
const State = {
    // État
    currentTab: 'COBOL',
    data: { programs: [], jcls: [], copybooks: [], datasets: [] },
    filteredData: [],
    selectedItem: null,
    searchQuery: '',
    
    // Méthodes
    setTab(tab) { /* ... */ },
    setData(type, data) { /* ... */ },
    setSelectedItem(item) { /* ... */ },
    search(query) { /* ... */ }
};
```

**Utilisation**:
```javascript
// Changer d'onglet
State.setTab('JCL');

// Écouter les changements
State.on('tabChanged', (tab) => {
    UIManager.renderTab(tab);
});
```

#### 2. **DataLoader** (`templates/scripts/data-loader.eta`)

Communication avec l'API REST.

```javascript
const DataLoader = {
    apiUrl: '<%= it.apiUrl %>',  // Injecté par ETA
    
    async loadAll() {
        const [programs, jcls, copybooks, datasets] = await Promise.all([
            this.fetchPrograms(),
            this.fetchJcls(),
            this.fetchCopybooks(),
            this.fetchDatasets()
        ]);
        return { programs, jcls, copybooks, datasets };
    },
    
    async fetchPrograms() {
        const response = await fetch(`${this.apiUrl}/programs`);
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return response.json();
    },
    
    // ... autres méthodes
};
```

**Gestion d'erreurs**:
```javascript
try {
    const data = await DataLoader.loadAll();
    State.setData('programs', data.programs);
} catch (error) {
    console.error('Erreur chargement:', error);
    UIManager.showError('Impossible de charger les données');
}
```

#### 3. **UIManager** (`templates/scripts/ui-manager.eta`)

Gestion de l'interface utilisateur.

```javascript
const UIManager = {
    // Rendu des cartes
    renderCards(items, type) {
        const container = document.getElementById('cards-container');
        container.innerHTML = items.map(item => 
            this.createCard(item, type)
        ).join('');
    },
    
    // Génération HTML d'une carte
    createCard(item, type) {
        return `
            <div class="card" data-id="${item.name}" data-type="${type}">
                <h3>${item.name}</h3>
                <div class="card-meta">
                    ${this.formatMeta(item, type)}
                </div>
            </div>
        `;
    },
    
    // Affichage détails
    showDetails(item, type) {
        const panel = document.getElementById('details-panel');
        panel.innerHTML = this.createDetailsHTML(item, type);
        panel.classList.add('visible');
    },
    
    // Recherche avec debounce
    setupSearch() {
        let timeout;
        document.getElementById('globalSearch').addEventListener('input', (e) => {
            clearTimeout(timeout);
            timeout = setTimeout(() => {
                State.search(e.target.value);
            }, 300);  // Debounce 300ms
        });
    }
};
```

#### 4. **App** (`templates/scripts/app.eta`)

Point d'entrée et initialisation.

```javascript
const App = {
    async init() {
        try {
            // 1. Charger les données
            UIManager.showLoading();
            const data = await DataLoader.loadAll();
            
            // 2. Initialiser l'état
            State.setData('programs', data.programs);
            State.setData('jcls', data.jcls);
            State.setData('copybooks', data.copybooks);
            State.setData('datasets', data.datasets);
            
            // 3. Setup UI
            UIManager.setupTabs();
            UIManager.setupSearch();
            UIManager.setupCardClicks();
            
            // 4. Afficher première vue
            UIManager.renderTab(State.currentTab);
            
            UIManager.hideLoading();
        } catch (error) {
            console.error('Init error:', error);
            UIManager.showError('Échec initialisation');
        }
    }
};

// Démarrage au chargement DOM
document.addEventListener('DOMContentLoaded', () => {
    App.init();
});
```

---

## 🎨 Styles et thème

### Variables CSS

**Dans `templates/styles.eta`**:
```css
:root {
    /* Couleurs principales */
    --bg-color: #1a1a2e;
    --card-bg: #16213e;
    --text-color: #eaeaea;
    --accent-color: #0f4c75;
    --highlight-color: #3282b8;
    
    /* Espacement */
    --spacing-sm: 8px;
    --spacing-md: 16px;
    --spacing-lg: 24px;
    
    /* Typographie */
    --font-main: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    --font-mono: 'Consolas', 'Monaco', monospace;
    
    /* Bordures */
    --border-radius: 8px;
    --border-color: #2d3748;
}
```

### Layout responsive

```css
/* Desktop */
.container {
    display: grid;
    grid-template-columns: 1fr 400px;  /* Liste + Détails */
    gap: var(--spacing-lg);
}

/* Tablet */
@media (max-width: 1024px) {
    .container {
        grid-template-columns: 1fr;
    }
    .details-panel {
        position: fixed;
        right: 0;
        top: 0;
        height: 100vh;
        width: 400px;
        transform: translateX(100%);
        transition: transform 0.3s;
    }
    .details-panel.visible {
        transform: translateX(0);
    }
}

/* Mobile */
@media (max-width: 768px) {
    .details-panel {
        width: 100%;
    }
}
```

### Composants UI

**Carte**:
```css
.card {
    background: var(--card-bg);
    border: 1px solid var(--border-color);
    border-radius: var(--border-radius);
    padding: var(--spacing-md);
    cursor: pointer;
    transition: all 0.2s;
}

.card:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
    border-color: var(--highlight-color);
}

.card h3 {
    margin: 0 0 var(--spacing-sm);
    color: var(--highlight-color);
    font-family: var(--font-mono);
}
```

**Badges**:
```css
.badge {
    display: inline-block;
    padding: 4px 8px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 600;
    background: var(--accent-color);
    color: white;
}

.badge.count {
    background: var(--highlight-color);
}
```

---

## 🔌 Communication API

### Endpoints utilisés

```javascript
const API_ENDPOINTS = {
    programs: '/api/programs',
    program: (name) => `/api/programs/${name}`,
    jcls: '/api/jcls',
    jcl: (name) => `/api/jcls/${name}`,
    copybooks: '/api/copybooks',
    copybook: (name) => `/api/copybooks/${name}`,
    datasets: '/api/datasets',
    dataset: (name) => `/api/datasets/${name}`
};
```

### Gestion des erreurs

```javascript
async function fetchWithError(url) {
    try {
        const response = await fetch(url);
        
        if (!response.ok) {
            throw new Error(`HTTP ${response.status}: ${response.statusText}`);
        }
        
        const data = await response.json();
        return data;
        
    } catch (error) {
        if (error.name === 'TypeError') {
            // Erreur réseau (API non accessible)
            throw new Error('API non accessible. Vérifier que l\'API tourne sur :8080');
        }
        throw error;
    }
}
```

### Loading states

```javascript
const UIManager = {
    showLoading() {
        document.body.classList.add('loading');
        document.getElementById('spinner').style.display = 'block';
    },
    
    hideLoading() {
        document.body.classList.remove('loading');
        document.getElementById('spinner').style.display = 'none';
    },
    
    showError(message) {
        const toast = document.createElement('div');
        toast.className = 'toast error';
        toast.textContent = message;
        document.body.appendChild(toast);
        
        setTimeout(() => toast.remove(), 5000);
    }
};
```

---

## 🔧 Modifier l'UI

### Changer les couleurs du thème

**Fichier**: `templates/styles.eta`

```css
:root {
    /* Modifier ces valeurs */
    --bg-color: #1a1a2e;      /* Fond principal */
    --card-bg: #16213e;       /* Fond cartes */
    --accent-color: #0f4c75;  /* Couleur accent */
    --highlight-color: #3282b8; /* Surbrillance */
}
```

**Rebuild**: `npm run build`

### Ajouter un nouvel onglet

**1. Modifier `templates/components/header.eta`**:
```html
<button class="tab" data-tab="Metrics">📊 Metrics</button>
```

**2. Ajouter données dans `templates/scripts/data-loader.eta`**:
```javascript
async fetchMetrics() {
    const response = await fetch(`${this.apiUrl}/metrics`);
    return response.json();
}

async loadAll() {
    // Ajouter dans Promise.all
    const metrics = await this.fetchMetrics();
    return { ..., metrics };
}
```

**3. Ajouter le rendu dans `templates/scripts/ui-manager.eta`**:
```javascript
renderTab(tab) {
    switch(tab) {
        case 'Metrics':
            this.renderMetrics(State.data.metrics);
            break;
        // ... autres cas
    }
}

renderMetrics(metrics) {
    const html = metrics.map(m => `
        <div class="metric-card">
            <h3>${m.name}</h3>
            <div class="value">${m.value}</div>
        </div>
    `).join('');
    document.getElementById('cards-container').innerHTML = html;
}
```

**4. Rebuild**: `npm run build`

### Modifier le format des cartes

**Fichier**: `templates/scripts/ui-manager.eta`

```javascript
createCard(item, type) {
    // Ajouter des infos, modifier le HTML
    return `
        <div class="card" data-id="${item.name}">
            <div class="card-header">
                <h3>${item.name}</h3>
                <span class="badge">${type}</span>
            </div>
            
            <!-- Nouvelle section -->
            <div class="card-stats">
                <span>📊 Complexité: ${item.complexity || 'N/A'}</span>
                <span>📏 Lignes: ${item.lines || 'N/A'}</span>
            </div>
            
            <div class="card-meta">
                ${this.formatMeta(item, type)}
            </div>
        </div>
    `;
}
```

### Ajouter un filtre

**Dans `templates/components/header.eta`**, ajouter:
```html
<select id="filter-select">
    <option value="all">Tous</option>
    <option value="with-calls">Avec CALL</option>
    <option value="no-calls">Sans CALL</option>
</select>
```

**Dans `templates/scripts/app.eta`**, ajouter listener:
```javascript
document.getElementById('filter-select').addEventListener('change', (e) => {
    const filter = e.target.value;
    State.applyFilter(filter);
});
```

**Dans `templates/scripts/state.eta`**, ajouter méthode:
```javascript
applyFilter(filter) {
    let filtered = this.data[this.currentTab.toLowerCase()];
    
    if (filter === 'with-calls') {
        filtered = filtered.filter(p => 
            p.callees && p.callees.length > 0
        );
    } else if (filter === 'no-calls') {
        filtered = filtered.filter(p => 
            !p.callees || p.callees.length === 0
        );
    }
    
    this.filteredData = filtered;
    this.emit('dataFiltered', filtered);
}
```

---

## ➕ Ajouter de nouvelles fonctionnalités

### Exemple: Graphe de dépendances visuel

**1. Ajouter D3.js dans `templates/index.eta`**:
```html
<script src="https://d3js.org/d3.v7.min.js"></script>
```

**2. Créer composant `templates/components/graph.eta`**:
```html
<div id="dependency-graph" class="graph-container">
    <svg id="graph-svg"></svg>
</div>

<style>
.graph-container {
    width: 100%;
    height: 600px;
    background: var(--card-bg);
    border-radius: var(--border-radius);
}
</style>
```

**3. Créer module `templates/scripts/graph.eta`**:
```javascript
const GraphRenderer = {
    renderDependencies(program) {
        const nodes = [
            { id: program.name, type: 'main' },
            ...program.callees.map(c => ({ id: c, type: 'callee' })),
            ...program.callers.map(c => ({ id: c, type: 'caller' }))
        ];
        
        const links = [
            ...program.callees.map(c => ({ 
                source: program.name, 
                target: c 
            })),
            ...program.callers.map(c => ({ 
                source: c, 
                target: program.name 
            }))
        ];
        
        // D3 force simulation
        const simulation = d3.forceSimulation(nodes)
            .force('link', d3.forceLink(links).id(d => d.id))
            .force('charge', d3.forceManyBody().strength(-200))
            .force('center', d3.forceCenter(400, 300));
        
        // Rendu SVG
        const svg = d3.select('#graph-svg');
        // ... code D3 pour dessiner
    }
};
```

**4. Intégrer dans le panneau détails**:
```javascript
// templates/scripts/ui-manager.eta
showDetails(item, type) {
    const panel = document.getElementById('details-panel');
    panel.innerHTML = `
        ${this.createDetailsHTML(item, type)}
        <div class="section">
            <h3>Graphe de dépendances</h3>
            <%~ include('components/graph') %>
        </div>
    `;
    
    // Rendu après insertion DOM
    setTimeout(() => {
        GraphRenderer.renderDependencies(item);
    }, 0);
}
```

### Exemple: Export CSV

```javascript
const Exporter = {
    toCSV(data, filename) {
        const headers = Object.keys(data[0]);
        const csv = [
            headers.join(','),
            ...data.map(row => 
                headers.map(h => 
                    JSON.stringify(row[h] || '')
                ).join(',')
            )
        ].join('\n');
        
        this.download(csv, filename, 'text/csv');
    },
    
    download(content, filename, type) {
        const blob = new Blob([content], { type });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = filename;
        a.click();
        URL.revokeObjectURL(url);
    }
};

// Utilisation
document.getElementById('export-btn').addEventListener('click', () => {
    Exporter.toCSV(State.data.programs, 'programs.csv');
});
```

---

## 🐛 Debugging

### Console navigateur

**Accès**: F12 ou Ctrl+Shift+I

**Commandes utiles**:
```javascript
// Inspecter l'état global
console.log(State);

// Voir toutes les données
console.table(State.data.programs);

// Tester une requête API
fetch('http://localhost:8080/api/programs/CBACT01C')
    .then(r => r.json())
    .then(console.log);

// Forcer un refresh
App.init();
```

### Logs de debug

**Ajouter dans `templates/scripts/app.eta`**:
```javascript
const DEBUG = true;

function log(...args) {
    if (DEBUG) console.log('[SMOJOL]', ...args);
}

// Utilisation
log('Loading programs:', data.programs.length);
log('Tab changed to:', tab);
```

### Erreurs courantes

**Problème**: Cartes ne s'affichent pas

**Solution**:
```javascript
// Vérifier les données
console.log('Data:', State.data);
console.log('Filtered:', State.filteredData);

// Vérifier le rendu
console.log('Container:', document.getElementById('cards-container'));
```

**Problème**: API ne répond pas

**Solution**:
```bash
# Tester l'API directement
curl http://localhost:8080/api/health

# Vérifier les logs API
# Devrait afficher: "Listening on http://localhost:8080/"
```

**Problème**: Recherche lente

**Solution**:
```javascript
// Augmenter le debounce
setTimeout(() => {
    State.search(e.target.value);
}, 500);  // 300ms → 500ms
```

### Network tab

**Chrome DevTools > Network**

- Voir toutes les requêtes API
- Temps de réponse
- Status codes
- Payloads JSON

---

## 📚 Ressources

### Documentation ETA

- Site officiel: https://eta.js.org/
- GitHub: https://github.com/eta-dev/eta
- Syntaxe: https://eta.js.org/docs/syntax

### JavaScript moderne

- MDN Web Docs: https://developer.mozilla.org/
- Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- ES6+ features: https://es6-features.org/

### CSS moderne

- CSS Variables: https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_custom_properties
- Flexbox: https://css-tricks.com/snippets/css/a-guide-to-flexbox/
- Grid: https://css-tricks.com/snippets/css/complete-guide-grid/

---

## 🎓 Bonnes pratiques

### Code style

- **Indentation**: 4 espaces
- **Quotes**: Single quotes `'string'`
- **Semicolons**: Oui
- **Naming**: camelCase pour JS, kebab-case pour CSS

### Performance

- ✅ Debounce sur recherche (300ms)
- ✅ Event delegation pour cartes
- ✅ Lazy loading des détails
- ✅ Cache des données en mémoire
- ❌ Éviter DOM queries dans loops

### Accessibilité

- ✅ Labels sur inputs
- ✅ Alt text sur images
- ✅ Keyboard navigation
- ✅ ARIA roles
- ✅ Contrastes couleurs

### SEO (si nécessaire)

- ✅ Meta tags
- ✅ Semantic HTML
- ✅ Progressive enhancement
- ❌ Pas de SSR (app interne)

---

## 🔄 Workflow de modification complet

### 1. Modifier le code

```bash
# Ouvrir dans VSCode
code templates/scripts/ui-manager.eta

# Faire les modifications...
```

### 2. Rebuild

```bash
npm run build
# ou en mode watch
npm run dev
```

### 3. Tester

```bash
# Terminal 1: API
cd smojol-rest-api
java -Dast.base.path=../out -jar target/smojol-rest-api-1.0.0.jar

# Terminal 2: UI
cd smojol-ui
python -m http.server 3000

# Navigateur
open http://localhost:3000
```

### 4. Debugging

- F12 → Console
- Vérifier les erreurs
- Tester les requêtes API
- Inspecter les éléments

### 5. Itérer

- Modifier template
- `npm run build`
- Refresh navigateur (Ctrl+R)

---

## ✅ Checklist avant déploiement

- [ ] `npm run build` sans erreurs
- [ ] Tous les onglets fonctionnent
- [ ] Recherche fonctionne
- [ ] Détails s'affichent correctement
- [ ] Pas d'erreurs dans Console
- [ ] API répond correctement
- [ ] Responsive (mobile, tablet)
- [ ] Thème dark cohérent
- [ ] Performance acceptable (<3s chargement)

---

**Documentation maintenue par**: Équipe Smojol  
**Questions**: Voir README.md ou contacter l'équipe
