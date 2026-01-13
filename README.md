# COBOL Analysis Quickstart Guide

This guide walks you through analyzing a COBOL/JCL project and viewing the results in the web UI.

## Overview

The analysis pipeline:

1. **Scans** your COBOL (.cbl), JCL (.jcl), and Copybook (.cpy) files
2. **Parses** JCL files to extract job/step/program relationships
3. **Generates ASTs** (Abstract Syntax Trees) for each COBOL program
4. **Extracts metadata** about copybook usage from the ASTs
5. **Produces JSON files** that power the web UI

```
┌─────────────────────────────────────────────────────────────────┐
│                     Your Mainframe Code                         │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐                     │
│  │  .cbl   │    │  .jcl   │    │  .cpy   │                     │
│  │ files   │    │ files   │    │ files   │                     │
│  └────┬────┘    └────┬────┘    └────┬────┘                     │
└───────┼──────────────┼──────────────┼───────────────────────────┘
        │              │              │
        └──────────────┼──────────────┘
                       ▼
        ┌──────────────────────────────┐
        │  scan_and_analyze_project.sh │
        └──────────────┬───────────────┘
                       │
        ┌──────────────┴───────────────┐
        │         Output (./out)        │
        │  • project-analysis.json      │
        │  • jcl-analysis.json          │
        │  • copybook-analysis.json     │
        │  • report/ (ASTs per program) │
        └──────────────┬───────────────┘
                       │
                       ▼
        ┌──────────────────────────────┐
        │        smojol-ui             │
        │    (Web-based Explorer)       │
        └──────────────────────────────┘
```

---

## Prerequisites

### 1. Java 21+

The COBOL parser requires Java 21 or higher.

```bash
# macOS (Homebrew)
brew install openjdk@21

# Verify
java -version
```

### 2. Python Virtual Environment

The JCL parser and some analysis scripts require Python with specific packages.

```bash
cd /path/to/cobol-rekt

# Create virtual environment
python3 -m venv .venv

# Activate it
source .venv/bin/activate   # macOS/Linux
# .venv\Scripts\activate    # Windows

# Install dependencies
pip install -r smojol_python/requirements.txt
pip install legacylens-jcl-parser
```

### 3. Build the Project (First Time Only)

```bash
# Build the Java components
mvn clean package -DskipTests
```

---

## Running the Analysis

### Basic Usage

```bash
# Make sure Java 21 is in your PATH
export PATH="/opt/homebrew/opt/openjdk@21/bin:$PATH"  # macOS with Homebrew

# Run the analysis
./scripts/scan_and_analyze_project.sh <cobol_dir> <jcl_dir> <copybook_dir> <output_dir>
```

### Example

```bash
./scripts/scan_and_analyze_project.sh \
    /path/to/project/app/cbl \
    /path/to/project/app/jcl \
    /path/to/project/app/cpy \
    ./out
```

### What Happens

| Step | What It Does |
|------|--------------|
| **Step 1** | Scans directories and counts files |
| **Step 2** | Creates CBL↔JCL mappings (which JCL calls which COBOL program) |
| **Step 3** | Generates aggregated ASTs for each COBOL program (includes copybook resolution) |
| **Step 4** | _(Optional)_ Generates dependency graphs |
| **Step 5** | Generates UI JSON files using the JCL parser |

### Output Files

After running, you'll have:

```
./out/
├── project-analysis.json          # CBL ↔ JCL mappings
├── jcl-analysis.json              # Parsed JCL files (jobs, steps, datasets)
├── copybook-analysis-complete.json # Programs with copybook metadata
└── report/
    ├── PROGRAM1.cbl.report/
    │   └── ast/
    │       └── aggregated/
    │           └── PROGRAM1-aggregated.json
    ├── PROGRAM2.cbl.report/
    │   └── ...
    └── ...
```

---

## Viewing Results in the UI

### Start a Local Server

```bash
cd /path/to/cobol-rekt

# Start Python's built-in HTTP server
python -m http.server 8080
```

### Open the UI

Navigate to:
```
http://localhost:8080/smojol-ui/?dataPath=./out
```

The UI lets you:
- Browse **COBOL programs** and see their JCL references and copybooks
- Browse **JCL files** and see their jobs, steps, and programs executed
- Browse **Copybooks** and see which programs use them
- Browse **Datasets** referenced in JCL

---

## Key Components

### What Each Script Does

| Script | Purpose |
|--------|---------|
| `scripts/scan_and_analyze_project.sh` | Main orchestrator - runs the full pipeline |
| `scripts/generate_ui_json.py` | Generates `jcl-analysis.json` and `copybook-analysis-complete.json` |
| `scripts/analyze_jcl_to_cobol.py` | Creates CBL↔JCL mappings (PGM= extraction) |

### What Each Output File Contains

| File | Contents |
|------|----------|
| `project-analysis.json` | Maps COBOL programs to the JCL files that execute them |
| `jcl-analysis.json` | Full JCL analysis: job names, steps, PGM references, DD statements |
| `copybook-analysis-complete.json` | For each program: which copybooks it uses and where |
| `report/*/ast/aggregated/*.json` | Full AST with inline copybook content and metadata |

### What Each Tool Does

| Tool | Purpose |
|------|---------|
| **smojol-cli** (Java) | Parses COBOL and generates ASTs with copybook resolution |
| **legacylens-jcl-parser** (Python) | AST-based parsing of JCL files |
| **smojol-ui** (HTML/JS) | Web interface to explore the analysis results |

---

## Optional Features

### Dependency Graphs

Generate visual dependency graphs (requires graphviz):

```bash
./scripts/scan_and_analyze_project.sh ... -g
```

### Performance Metrics

Generate detailed timing metrics:

```bash
./scripts/scan_and_analyze_project.sh ... -m
```

---

## Troubleshooting

### "Java version too low"

```bash
# Check your Java version
java -version

# Make sure Java 21+ is first in PATH
export PATH="/opt/homebrew/opt/openjdk@21/bin:$PATH"
```

### "jcl_parser module not found"

```bash
# Make sure venv is activated and package installed
source .venv/bin/activate
pip install legacylens-jcl-parser
```

### "smojol-cli.jar not found"

```bash
# Build the project first
mvn clean package -DskipTests
```

### UI shows "Failed to load data"

1. Make sure the server is running from the project root
2. Check the dataPath parameter matches your output directory
3. Verify all three JSON files exist in the output directory

---

## What This Repo Is NOT For

This repository contains many advanced features for COBOL reverse engineering (flowcharts, Neo4J integration, LLM analysis, etc.). 

**For basic project analysis and visualization, you only need:**
- `scripts/scan_and_analyze_project.sh`
- `scripts/generate_ui_json.py`
- `smojol-ui/`

The other modules (`smojol-toolkit`, `smojol-core`, etc.) power the underlying analysis but you don't need to interact with them directly.
