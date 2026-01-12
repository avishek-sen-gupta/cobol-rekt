#!/opt/homebrew/bin/bash

################################################################################
# scan_and_analyze_project.sh
# 
# Orchestrates complete project analysis pipeline:
# 1. Scans directory structure (CBL, JCL, CPY files) - supports modular projects
# 2. Creates CBL <-> JCL mappings (optional, AST generated even without JCL)
# 3. Generates aggregated ASTs for all programs
# 4. Builds dependency graph from ASTs and JCL relationships
#
# Usage:
#   bash scripts/scan_and_analyze_project.sh <root_dir> [output_dir] [options]
#   bash scripts/scan_and_analyze_project.sh <cobol_dir> <jcl_dir> <cpy_dir> [output_dir] [options]
#
# Modes:
#   • MODULAR:  Supports app-*/cbl/, app-*/jcl/, app-*/cpy/ structure
#   • FLAT:     Supports separate cbl/, jcl/, cpy/ directories (legacy)
#
# Options:
#   -g, --graph       Generate dependency graphs (JSON + SVG) - disabled by default
#   -m, --metrics     Generate performance metrics report - disabled by default
#
# Example:
#   # Modular structure (auto-detect)
#   bash scripts/scan_and_analyze_project.sh /path/to/aws-mainframe ./out
#
#   # Flat structure
#   bash scripts/scan_and_analyze_project.sh app/cbl app/jcl app/cpy ./out -g -m
################################################################################

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Arguments
ROOT_DIR="$1"
OUTPUT_DIR=""
COBOL_DIR=""
JCL_DIR=""
CPY_DIR=""

# Detect mode: modular or flat
# Modular: app-module1/cbl, app-module1/jcl, app-module1/cpy
# Flat: cbl/, jcl/, cpy/ directories
if [[ -z "$ROOT_DIR" ]]; then
    echo -e "${RED}ERROR: Missing root directory${NC}"
    exit 1
fi

# Check if this is a flat structure (all 3 different args provided AND they're actual cbl/jcl/cpy dirs)
if [[ -n "$2" ]] && [[ -n "$3" ]] && [[ ! "$2" =~ ^- ]] && [[ ! "$3" =~ ^- ]] && \
   [[ "$1" != "$2" || "$2" != "$3" ]]; then
    # Arguments are different: probably cbl_dir, jcl_dir, cpy_dir
    # Verify they contain the expected files
    cbl_in_arg1=$(find "$1" -maxdepth 1 -name "*.cbl" -type f 2>/dev/null | wc -l)
    jcl_in_arg2=$(find "$2" -maxdepth 1 -name "*.jcl" -type f 2>/dev/null | wc -l)
    cpy_in_arg3=$(find "$3" -maxdepth 1 -name "*.cpy" -type f 2>/dev/null | wc -l)
    
    if [[ $cbl_in_arg1 -gt 0 ]] || [[ $jcl_in_arg2 -gt 0 ]] || [[ $cpy_in_arg3 -gt 0 ]]; then
        # FLAT MODE: explicit cbl, jcl, cpy directories
        COBOL_DIR="$1"
        JCL_DIR="$2"
        CPY_DIR="$3"
        OUTPUT_DIR="${4:-./out}"
        MODE="FLAT"
    else
        # Arguments are different but don't look like cbl/jcl/cpy dirs
        # Fall back to MODULAR
        COBOL_DIR="$1"
        JCL_DIR="$1"
        CPY_DIR="$1"
        OUTPUT_DIR="${2:-./out}"
        MODE="MODULAR"
    fi
else
    # Only 1 argument or identical arguments: MODULAR MODE
    COBOL_DIR="$1"
    JCL_DIR="$1"
    CPY_DIR="$1"
    OUTPUT_DIR="${2:-./out}"
    MODE="MODULAR"
    
    # If 3 identical args and a 4th arg, use that as output
    if [[ -n "$4" ]] && [[ ! "$4" =~ ^- ]]; then
        OUTPUT_DIR="$4"
    fi
fi

# Parse optional flags (disabled by default)
GENERATE_GRAPHS=false
GENERATE_METRICS=false

# Determine which argument to start parsing flags from
if [[ "$MODE" == "FLAT" ]]; then
    START_ARG=5  # cbl_dir jcl_dir cpy_dir output_dir [flags...]
else
    # MODULAR: check if arg 3 is the output dir (then flags start at arg 4)
    # or if arg 3 is a flag (then flags start at arg 3)
    if [[ -n "$3" ]] && [[ "$3" =~ ^- ]]; then
        START_ARG=3
    elif [[ -n "$4" ]] && [[ "$4" =~ ^- ]]; then
        START_ARG=4
    else
        START_ARG=5  # Fallback for edge cases
    fi
fi

for arg in "${@:$START_ARG}"; do
    case "$arg" in
        -g|--graph)
            GENERATE_GRAPHS=true
            ;;
        -m|--metrics)
            GENERATE_METRICS=true
            ;;
    esac
done

# Validate inputs
[[ -d "$COBOL_DIR" ]] || { echo -e "${RED}ERROR: COBOL directory not found: $COBOL_DIR${NC}"; exit 1; }
[[ -d "$JCL_DIR" ]] || { echo -e "${RED}ERROR: JCL directory not found: $JCL_DIR${NC}"; exit 1; }
[[ -d "$CPY_DIR" ]] || { echo -e "${RED}ERROR: Copybook directory not found: $CPY_DIR${NC}"; exit 1; }

mkdir -p "$OUTPUT_DIR"

echo -e "${BLUE}╔═══════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║     COBOL Project Analysis & AST Generation          ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════╝${NC}"
echo ""

# ============================================================================
# STEP 1: SCAN PROJECT STRUCTURE
# ============================================================================

echo -e "${BLUE}[Step 1/4] Scanning project structure${NC}"
echo "  Mode:       $MODE"
echo "  Root:       $COBOL_DIR"
echo "  Output:     $OUTPUT_DIR"
echo ""

# Find all CBL files with their module context
declare -A CBL_FILES_MAP  # Map: CBL_PATH -> MODULE_PATH
declare -A CBL_MODULE_MAP # Map: CBL_NAME -> MODULE_PATH

if [[ "$MODE" == "MODULAR" ]]; then
    # Find all CBL files in modular structure (app-*/cbl/, cbl/)
    for cbl_file in $(find "$COBOL_DIR" -path "*/cbl/*.cbl" -type f); do
        cbl_name=$(basename "$cbl_file" .cbl)
        # Extract module path (e.g., /path/to/app-module1 from /path/to/app-module1/cbl/file.cbl)
        module_path=$(echo "$cbl_file" | sed 's|/cbl/[^/]*$||')
        CBL_FILES_MAP["$cbl_file"]="$module_path"
        CBL_MODULE_MAP["$cbl_name"]="$module_path"
    done
else
    # Find all CBL files in flat structure
    for cbl_file in $(find "$COBOL_DIR" -maxdepth 1 -name "*.cbl" -type f); do
        cbl_name=$(basename "$cbl_file" .cbl)
        CBL_FILES_MAP["$cbl_file"]="$COBOL_DIR"
        CBL_MODULE_MAP["$cbl_name"]="$COBOL_DIR"
    done
fi

CBL_COUNT=${#CBL_FILES_MAP[@]}

# Count JCL and CPY files
JCL_COUNT=$(find "$JCL_DIR" -name "*.jcl" -type f 2>/dev/null | wc -l)
CPY_COUNT=$(find "$CPY_DIR" -name "*.cpy" -type f 2>/dev/null | wc -l)

echo "  Found: $CBL_COUNT CBL files, $JCL_COUNT JCL files, $CPY_COUNT copybooks"
echo ""

# ============================================================================
# STEP 2: CREATE CBL-JCL MAPPINGS (Using Python script for accurate matching)
# ============================================================================

echo -e "${BLUE}[Step 2/4] Creating CBL-JCL mappings${NC}"

MAPPINGS_FILE="$OUTPUT_DIR/project-analysis.json"
mkdir -p "$OUTPUT_DIR"

JCL_ANALYSIS_SCRIPT="$SCRIPT_DIR/analyze_jcl_to_cobol.py"

# Check for virtual environment with jcl-parser installed
VENV_PYTHON=""
if [[ -f "$PROJECT_ROOT/.venv/bin/python" ]]; then
    VENV_PYTHON="$PROJECT_ROOT/.venv/bin/python"
elif [[ -f "$PROJECT_ROOT/.venv/Scripts/python.exe" ]]; then
    # Windows
    VENV_PYTHON="$PROJECT_ROOT/.venv/Scripts/python.exe"
fi

# Try to find Python (prefer venv with jcl-parser)
PYTHON_CMD=""
if [[ -n "$VENV_PYTHON" ]]; then
    PYTHON_CMD="$VENV_PYTHON"
    echo "  Using venv Python with AST-based JCL parser"
else
    for cmd in python python3 python2; do
        if command -v "$cmd" &>/dev/null 2>&1; then
            if "$cmd" --version &>/dev/null 2>&1; then
                PYTHON_CMD="$cmd"
                break
            fi
        fi
    done
    if [[ -n "$PYTHON_CMD" ]]; then
        echo "  Using system Python (regex fallback for JCL parsing)"
    fi
fi

if [[ -n "$PYTHON_CMD" ]] && [[ -f "$JCL_ANALYSIS_SCRIPT" ]]; then
    # Convert paths to Windows format for Python if on Windows/Git Bash
    PYTHON_JCL_DIR="$JCL_DIR"
    PYTHON_COBOL_DIR="$COBOL_DIR"
    
    if [[ "$JCL_DIR" =~ ^/c/ ]]; then
        PYTHON_JCL_DIR=$(echo "$JCL_DIR" | sed 's|^/c/|C:/|' | sed 's|/|\\|g')
    fi
    if [[ "$COBOL_DIR" =~ ^/c/ ]]; then
        PYTHON_COBOL_DIR=$(echo "$COBOL_DIR" | sed 's|^/c/|C:/|' | sed 's|/|\\|g')
    fi
    
    # Use Python script for accurate JCL-COBOL matching via PGM= extraction
    TEMP_OUTPUT=$(mktemp)
    "$PYTHON_CMD" "$JCL_ANALYSIS_SCRIPT" \
        -j "$PYTHON_JCL_DIR" \
        -c "$PYTHON_COBOL_DIR" \
        --json > "$TEMP_OUTPUT" 2>&1
    PYTHON_EXIT=$?
    
    if [[ $PYTHON_EXIT -eq 0 ]] && [[ -s "$TEMP_OUTPUT" ]]; then
        cp "$TEMP_OUTPUT" "$MAPPINGS_FILE"
        rm "$TEMP_OUTPUT"
        
        # Get count before any further processing
        MAPPED=$(grep -c '"program"' "$MAPPINGS_FILE" 2>/dev/null || echo "0")
        echo "  Matched: $MAPPED programs using PGM= extraction"
    else
        echo -e "${YELLOW}  Warning: Python script analysis failed${NC}"
        echo "{}" > "$MAPPINGS_FILE"
        MAPPED=0
    fi
else
    if [[ -z "$PYTHON_CMD" ]]; then
        echo -e "${YELLOW}  Warning: Python not found${NC}"
    fi
    echo "{}" > "$MAPPINGS_FILE"
    MAPPED=0
fi

echo ""

# ============================================================================
# STEP 3: GENERATE AGGREGATED ASTs
# ============================================================================

echo -e "${BLUE}[Step 3/4] Generating aggregated ASTs${NC}"

REPORT_DIR="$OUTPUT_DIR/report"
mkdir -p "$REPORT_DIR"

# Check if smojol-cli JAR is available
JAR_PATH="$PROJECT_ROOT/smojol-cli/target/smojol-cli.jar"
AST_COUNT=0

# Timing and statistics
AST_START_TIME=$(date +%s%N)  # Nanoseconds for better precision
TOTAL_LOC=0

# Per-file metrics array
declare -a FILE_METRICS  # Array to store metrics for each file

# Load JCL-CBL mappings from Step 2
declare -A JCL_MAPPINGS
if [[ -f "$MAPPINGS_FILE" ]]; then
    # Parse JSON to extract JCL files for each program
    # Using Python for reliable JSON parsing
    PYTHON_CMD=""
    for cmd in python python3; do
        if command -v "$cmd" &>/dev/null 2>&1; then
            PYTHON_CMD="$cmd"
            break
        fi
    done
    
    if [[ -n "$PYTHON_CMD" ]]; then
        # Convert path to Windows format for Python if on Windows/Git Bash
        PYTHON_MAPPING_FILE="$MAPPINGS_FILE"
        if [[ "$MAPPINGS_FILE" =~ ^/c/ ]]; then
            # Convert /c/path to C:\path for Python
            PYTHON_MAPPING_FILE=$(echo "$MAPPINGS_FILE" | sed 's|^/c/|C:/|' | sed 's|/|\\|g')
        fi
        
        # Load mappings: program -> jcl_files array
        eval "$("$PYTHON_CMD" -c "
import json
with open(r'$PYTHON_MAPPING_FILE', 'r') as f:
    data = json.load(f)
for prog_data in data.get('cbl_files', []):
    prog = prog_data['program'].upper()
    jcl_list = prog_data.get('jcl_files', [])
    if jcl_list:
        jcls = '|'.join(jcl_list)
        print(f'JCL_MAPPINGS[{prog}]=\"{jcls}\"')
" 2>/dev/null)"
    fi
fi

if [[ -f "$JAR_PATH" ]] && command -v java &>/dev/null; then
    # Determine the correct copybook directory
    CPY_SEARCH_DIR="$CPY_DIR"
    TEMP_CPY_DIR=""
    
    # Count copybooks at top level
    cpy_count=$(find "$CPY_DIR" -maxdepth 1 -name "*.cpy" -type f 2>/dev/null | wc -l)
    
    # If no copybooks at top level, check for cpy subdirectory
    if [[ $cpy_count -eq 0 ]] && [[ -d "$CPY_DIR/cpy" ]]; then
        CPY_SEARCH_DIR="$CPY_DIR/cpy"
        cpy_count=$(find "$CPY_SEARCH_DIR" -maxdepth 1 -name "*.cpy" -type f 2>/dev/null | wc -l)
    fi
    
    # For modular structure: aggregate all copybooks to temp directory
    if [[ $cpy_count -eq 0 ]]; then
        total_cpy=$(find "$CPY_DIR" -name "*.cpy" -type f 2>/dev/null | wc -l)
        if [[ $total_cpy -gt 0 ]]; then
            TEMP_CPY_DIR=$(mktemp -d)
            find "$CPY_DIR" -name "*.cpy" -type f -exec cp {} "$TEMP_CPY_DIR/" \;
            CPY_SEARCH_DIR="$TEMP_CPY_DIR"
            cpy_count=$total_cpy
        fi
    fi
    
    # Use smojol-cli to generate proper ASTs with WRITE_AGGREGATED_JCL_AST
    for cbl_file in "${!CBL_FILES_MAP[@]}"; do
        if [[ ! -f "$cbl_file" ]]; then continue; fi
        
        cbl_name=$(basename "$cbl_file" .cbl)
        cbl_name_upper=$(echo "$cbl_name" | tr '[:lower:]' '[:upper:]')
        module_path="${CBL_FILES_MAP[$cbl_file]}"
        
        # Count lines of code for this file
        file_loc=$(wc -l < "$cbl_file" 2>/dev/null || echo 0)
        ((TOTAL_LOC += file_loc))
        
        # Get file size in bytes
        file_size=$(stat -f%z "$cbl_file" 2>/dev/null || stat -c%s "$cbl_file" 2>/dev/null || echo 0)
        
        # Start timing for this file
        FILE_START=$(date +%s%N)
        
        # Step 1: Find copybooks for this file
        # Priority: module's cpy/ > global cpy/
        CPY_SEARCH_DIR="$CPY_DIR"
        TEMP_CPY_DIR=""
        
        if [[ "$MODE" == "MODULAR" ]]; then
            # Check for cpy directory in the same module
            MODULE_CPY_DIR="$module_path/cpy"
            if [[ -d "$MODULE_CPY_DIR" ]]; then
                module_cpy_count=$(find "$MODULE_CPY_DIR" -name "*.cpy" -type f 2>/dev/null | wc -l)
                
                if [[ $module_cpy_count -gt 0 ]]; then
                    # Use module's copybooks + global copybooks
                    TEMP_CPY_DIR=$(mktemp -d)
                    
                    # Copy module's copybooks
                    find "$MODULE_CPY_DIR" -name "*.cpy" -type f -exec cp {} "$TEMP_CPY_DIR/" \; 2>/dev/null
                    
                    # Copy global copybooks (to have access to shared copybooks)
                    find "$CPY_DIR" -name "*.cpy" -type f -exec cp {} "$TEMP_CPY_DIR/" \; 2>/dev/null
                    
                    CPY_SEARCH_DIR="$TEMP_CPY_DIR"
                fi
            fi
        fi
        
        # If still no copybooks found, aggregate all available
        if [[ -z "$TEMP_CPY_DIR" ]]; then
            cpy_count=$(find "$CPY_SEARCH_DIR" -maxdepth 1 -name "*.cpy" -type f 2>/dev/null | wc -l)
            
            if [[ $cpy_count -eq 0 ]]; then
                total_cpy=$(find "$CPY_DIR" -name "*.cpy" -type f 2>/dev/null | wc -l)
                if [[ $total_cpy -gt 0 ]]; then
                    TEMP_CPY_DIR=$(mktemp -d)
                    find "$CPY_DIR" -name "*.cpy" -type f -exec cp {} "$TEMP_CPY_DIR/" \; 2>/dev/null
                    CPY_SEARCH_DIR="$TEMP_CPY_DIR"
                fi
            fi
        fi
        
        # Step 2: Find JCL for this file (optional)
        # Priority: module's jcl/ > mappings > global jcl/
        PROGRAM_JCL_DIR="$JCL_DIR"
        TEMP_PROGRAM_JCL_DIR=""
        JCL_FOUND=0
        
        if [[ "$MODE" == "MODULAR" ]]; then
            MODULE_JCL_DIR="$module_path/jcl"
            if [[ -d "$MODULE_JCL_DIR" ]]; then
                # Check for matching JCL file in module
                matching_jcl=$(find "$MODULE_JCL_DIR" -iname "*$cbl_name*" -name "*.jcl" -type f 2>/dev/null | head -1)
                if [[ -f "$matching_jcl" ]]; then
                    TEMP_PROGRAM_JCL_DIR=$(mktemp -d)
                    cp "$matching_jcl" "$TEMP_PROGRAM_JCL_DIR/" 2>/dev/null
                    PROGRAM_JCL_DIR="$TEMP_PROGRAM_JCL_DIR"
                    JCL_FOUND=1
                fi
            fi
        fi
        
        # If no module JCL, check global mappings
        if [[ $JCL_FOUND -eq 0 ]] && [[ -n "${JCL_MAPPINGS[$cbl_name_upper]}" ]]; then
            TEMP_PROGRAM_JCL_DIR=$(mktemp -d)
            IFS='|' read -ra jcl_files <<< "${JCL_MAPPINGS[$cbl_name_upper]}"
            
            for jcl_file_path in "${jcl_files[@]}"; do
                full_jcl_path="$JCL_DIR/$jcl_file_path"
                
                if [[ ! -f "$full_jcl_path" ]]; then
                    jcl_basename=$(basename "$jcl_file_path")
                    full_jcl_path=$(find "$JCL_DIR" -iname "$jcl_basename" -type f 2>/dev/null | head -1)
                fi
                
                if [[ -f "$full_jcl_path" ]]; then
                    cp "$full_jcl_path" "$TEMP_PROGRAM_JCL_DIR/" 2>/dev/null || true
                    ((JCL_FOUND++))
                fi
            done
            
            if [[ $JCL_FOUND -gt 0 ]]; then
                PROGRAM_JCL_DIR="$TEMP_PROGRAM_JCL_DIR"
            fi
        fi
        
        # Step 3: Generate AST (with or without JCL)
        # Use the module root as source for modular, or cbl_dir for flat
        if [[ "$MODE" == "MODULAR" ]]; then
            CBL_SOURCE_DIR="$module_path"
        else
            CBL_SOURCE_DIR="$(dirname "$cbl_file")"
        fi
        
        ERROR_LOG=$(mktemp)
        if java -jar "$JAR_PATH" run \
            -c WRITE_AGGREGATED_JCL_AST \
            -j "$PROGRAM_JCL_DIR" \
            -s "$CBL_SOURCE_DIR" \
            -cp "$CPY_SEARCH_DIR" \
            -r "$REPORT_DIR" \
            "$cbl_name.cbl" > "$ERROR_LOG" 2>&1; then
            ((AST_COUNT++))
            FILE_STATUS="✓"
            rm "$ERROR_LOG"
        else
            FILE_STATUS="✗"
            # Store error details for later inspection if metrics enabled
            # ERROR_DETAILS=$(head -1 "$ERROR_LOG" 2>/dev/null)
            rm "$ERROR_LOG"
        fi
        
        # End timing for this file
        FILE_END=$(date +%s%N)
        FILE_DURATION_MS=$(( (FILE_END - FILE_START) / 1000000 ))
        
        # Store metrics for later reporting
        FILE_METRICS+=("$cbl_name|$file_loc|$file_size|$FILE_DURATION_MS|$FILE_STATUS")
        
        # Clean up temporary directories
        [[ -n "$TEMP_PROGRAM_JCL_DIR" && -d "$TEMP_PROGRAM_JCL_DIR" ]] && rm -rf "$TEMP_PROGRAM_JCL_DIR"
        [[ -n "$TEMP_CPY_DIR" && -d "$TEMP_CPY_DIR" ]] && rm -rf "$TEMP_CPY_DIR"
    done
else
    # Count lines of code even for fallback
    for cbl_file in "${!CBL_FILES_MAP[@]}"; do
        file_loc=$(wc -l < "$cbl_file" 2>/dev/null || echo 0)
        ((TOTAL_LOC += file_loc))
    done
    
    # Fallback: Generate minimal AST structure if JAR unavailable
    if [[ ! -f "$JAR_PATH" ]]; then
        echo -e "${YELLOW}  Warning: smojol-cli.jar not found${NC}"
    fi
    if ! command -v java &>/dev/null; then
        echo -e "${YELLOW}  Warning: Java not available${NC}"
    fi
    echo "  Generating minimal AST structure as fallback"
    
    for cbl_file in "${!CBL_FILES_MAP[@]}"; do
        if [[ ! -f "$cbl_file" ]]; then continue; fi
        
        cbl_name=$(basename "$cbl_file" .cbl)
        file_loc=$(wc -l < "$cbl_file" 2>/dev/null || echo 0)
        file_size=$(stat -f%z "$cbl_file" 2>/dev/null || stat -c%s "$cbl_file" 2>/dev/null || echo 0)
        
        # Timing
        FILE_START=$(date +%s%N)
        FILE_END=$(date +%s%N)
        FILE_DURATION_MS=$(( (FILE_END - FILE_START) / 1000000 ))
        
        ast_dir="$REPORT_DIR/$cbl_name.cbl.report/ast/aggregated"
        mkdir -p "$ast_dir"
        
        # Create minimal AST JSON with basic structure
        cat > "$ast_dir/$cbl_name-aggregated.json" <<'EOF'
{
  "nodeType": "StartRuleContext",
  "text": "COBOL program stub - full AST generation requires smojol-cli JAR",
  "copybooks": [],
  "children": [
    {
      "nodeType": "CompilationUnitContext",
      "text": "Fallback AST",
      "copybooks": [],
      "children": []
    }
  ]
}
EOF
        ((AST_COUNT++))
        FILE_METRICS+=("$cbl_name|$file_loc|$file_size|$FILE_DURATION_MS|✓")
    done
fi

echo "  Generated: $AST_COUNT aggregated AST files"
echo ""

# Calculate timing BEFORE metrics generation
AST_END_TIME=$(date +%s%N)
AST_DURATION_SEC=$(( (AST_END_TIME - AST_START_TIME) / 1000000000 ))
AST_MINUTES=$((AST_DURATION_SEC / 60))
AST_SECONDS=$((AST_DURATION_SEC % 60))

# ============================================================================
# GENERATE PERFORMANCE METRICS REPORT (OPTIONAL)
# ============================================================================

METRICS_FILE="$OUTPUT_DIR/performance-metrics.txt"
METRICS_JSON="$OUTPUT_DIR/performance-metrics.json"

if [[ "$GENERATE_METRICS" == "true" ]]; then
    # Generate text metrics file
    {
        echo "COBOL AST Generation - Performance Metrics Report"
        echo "=================================================="
        echo ""
        echo "Per-File Metrics:"
        echo ""
        echo "  File Name              LOC    Size      Time(ms)  Status"
        echo "  ────────────────────────────────────────────────────────"
        
        for metric in "${FILE_METRICS[@]}"; do
            IFS='|' read -r fname loc size time status <<< "$metric"
            printf "  %-24s %5d  %7d   %8d   %s\n" "$fname" "$loc" "$size" "$time" "$status"
        done
        
        echo ""
        echo "Summary Metrics:"
        echo "  • Total files processed:  $AST_COUNT"
        echo "  • Total LOC:              $TOTAL_LOC"
        echo "  • Total execution time:   ${AST_MINUTES}m ${AST_SECONDS}s ($AST_DURATION_SEC seconds)"
        
        if [[ $TOTAL_LOC -gt 0 ]]; then
            AVG_TIME_PER_LOC=$((AST_DURATION_SEC * 1000 / TOTAL_LOC))
            PROCESSING_RATE=$((TOTAL_LOC / (AST_DURATION_SEC + 1)))
            echo "  • Time per 1000 LOC:      ~${AVG_TIME_PER_LOC}ms"
            echo "  • Processing rate:        ~${PROCESSING_RATE} LOC/sec"
        fi
        echo ""
        echo "Generated at: $(date)"
    } > "$METRICS_FILE"

    # Generate metrics file only if enabled

    {
        echo "{"
        echo '  "metrics": ['
        
        FIRST=true
        for metric in "${FILE_METRICS[@]}"; do
            IFS='|' read -r fname loc size time status <<< "$metric"
            
            if [[ "$FIRST" == "true" ]]; then
                FIRST=false
            else
                echo ","
            fi
            
            printf '    {"file": "%s", "loc": %d, "bytes": %d, "time_ms": %d, "status": "%s"}' \
                   "$fname" "$loc" "$size" "$time" "$status"
        done
        
        echo ""
        echo "  ],"
        echo "  \"summary\": {"
        echo "    \"total_files\": $AST_COUNT,"
        echo "    \"total_loc\": $TOTAL_LOC,"
        echo "    \"total_time_seconds\": $AST_DURATION_SEC,"
        
        if [[ $TOTAL_LOC -gt 0 ]]; then
            AVG_TIME_PER_LOC=$((AST_DURATION_SEC * 1000 / TOTAL_LOC))
            PROCESSING_RATE=$((TOTAL_LOC / (AST_DURATION_SEC + 1)))
            echo "    \"avg_time_per_1000_loc_ms\": $AVG_TIME_PER_LOC,"
            echo "    \"processing_rate_loc_per_sec\": $PROCESSING_RATE,"
        fi
        
        echo "    \"timestamp\": \"$(date)\""
        echo "  }"
        echo "}"
    } > "$METRICS_JSON"
fi

echo ""

# ============================================================================
# STEP 4-5: GENERATE DEPENDENCY GRAPHS (JSON + SVG) - OPTIONAL
# ============================================================================

if [[ "$GENERATE_GRAPHS" == "true" ]]; then
    echo -e "${BLUE}[Step 4-5] Generating dependency graphs${NC}"

    # Find Python interpreter
    PYTHON_CMD=""
    for cmd in python python3 python2; do
        if command -v "$cmd" &>/dev/null 2>&1 && "$cmd" --version &>/dev/null 2>&1; then
            PYTHON_CMD="$cmd"
            break
        fi
    done

    if [[ -z "$PYTHON_CMD" ]]; then
        echo "  Warning: Python not found, skipping graph generation"
    else
        # Generate global dependency graph (JSON format)
        GRAPH_FILE="$OUTPUT_DIR/dependency-graph.json"
        GRAPH_SCRIPT="$SCRIPT_DIR/generate_dependency_graph_json.py"
        
        if [[ -f "$GRAPH_SCRIPT" ]]; then
            if "$PYTHON_CMD" "$GRAPH_SCRIPT" \
                --cobol-dir "$COBOL_DIR" \
                --jcl-dir "$JCL_DIR" \
                --ast-dir "$REPORT_DIR" \
                --output "$GRAPH_FILE" 2>/dev/null; then
                
                if [[ -f "$GRAPH_FILE" ]]; then
                    GRAPH_SIZE=$(du -h "$GRAPH_FILE" 2>/dev/null | awk '{print $1}')
                    echo "  Global dependency graph: $GRAPH_SIZE"
                fi
            fi
        fi
        
        # Generate individual program graphs (SVG format)
        PROGRAM_GRAPHS_DIR="$OUTPUT_DIR/program-graphs"
        mkdir -p "$PROGRAM_GRAPHS_DIR"
        
        SVG_SCRIPT="$SCRIPT_DIR/generate_graph_from_aggregated.py"
        if [[ -f "$SVG_SCRIPT" ]]; then
            AST_FILES=$(find "$REPORT_DIR" -name "*-aggregated.json" -type f 2>/dev/null)
            
            while IFS= read -r ast_file; do
                if [[ -z "$ast_file" ]]; then continue; fi
                
                PROGRAM_NAME=$(basename "$ast_file" "-aggregated.json")
                OUTPUT_SVG="$PROGRAM_GRAPHS_DIR/${PROGRAM_NAME}_dependencies.svg"
                
                "$PYTHON_CMD" "$SVG_SCRIPT" \
                    --input "$ast_file" \
                    --output "$OUTPUT_SVG" >/dev/null 2>&1
            done <<< "$AST_FILES"
            
            SVG_COUNT=$(find "$PROGRAM_GRAPHS_DIR" -name "*_dependencies.svg" -type f 2>/dev/null | wc -l)
            if [[ $SVG_COUNT -gt 0 ]]; then
                echo "  Generated: $SVG_COUNT individual program graphs"
            fi
        fi
    fi
else
    echo -e "${BLUE}[Step 4-5] Dependency graph generation disabled (use -g to enable)${NC}"
fi

echo ""

# ============================================================================
# SUMMARY
# ============================================================================

echo -e "${BLUE}╔═══════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║            Analysis Complete                         ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════╝${NC}"
echo ""

echo "Analysis Summary:"
echo "  Files analyzed:    $CBL_COUNT COBOL, $JCL_COUNT JCL, $CPY_COUNT Copybooks"
echo "  Programs mapped:   $MAPPED of $CBL_COUNT"
echo "  ASTs generated:    $AST_COUNT"
echo "  Total LOC:         $TOTAL_LOC"
echo "  Execution time:    ${AST_MINUTES}m ${AST_SECONDS}s"
echo ""

# Display per-file metrics only if enabled
if [[ "$GENERATE_METRICS" == "true" ]]; then
    echo "Per-File Performance Metrics:"
    echo ""
    echo "  File Name              LOC    Size      Time(ms)  Status"
    echo "  ────────────────────────────────────────────────────────"

    for metric in "${FILE_METRICS[@]}"; do
        IFS='|' read -r fname loc size time status <<< "$metric"
        printf "  %-24s %5d  %7d   %8d   %s\n" "$fname" "$loc" "$size" "$time" "$status"
    done

    echo ""
    if [[ $TOTAL_LOC -gt 0 ]]; then
        echo "  Scalability Metrics:"
        echo "    • Total LOC processed:  $TOTAL_LOC"
        echo "    • Time per 1000 LOC:    ~$((AST_DURATION_SEC * 1000 / TOTAL_LOC))ms"
        echo "    • Processing rate:      ~$((TOTAL_LOC / (AST_DURATION_SEC + 1))) LOC/sec"
        echo ""
    fi
fi

echo "Output directories:"
echo "  📊 Mappings:      $MAPPINGS_FILE"
echo "  📁 ASTs:          $REPORT_DIR"
if [[ "$GENERATE_METRICS" == "true" ]]; then
    echo "  📈 Metrics:       $METRICS_FILE"
    echo "  📋 Metrics JSON:  $METRICS_JSON"
fi
if [[ "$GENERATE_GRAPHS" == "true" ]]; then
    echo "  🔗 Graphs:        $PROGRAM_GRAPHS_DIR"
fi
echo ""
echo ""

echo "Next steps:"
echo "  1. Review mappings: cat $MAPPINGS_FILE | jq ."
echo "  2. Check ASTs:      ls -la $REPORT_DIR"
if [[ "$GENERATE_METRICS" == "true" ]]; then
    echo "  3. View metrics:    cat $METRICS_FILE"
    echo "  4. View metrics JSON: cat $METRICS_JSON | jq ."
fi
echo ""
