#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Analyse JCL → COBOL: Partir des JCL pour tracer les fichiers COBOL
GÉNÉRIQUE: Scanne récursivement tous les répertoires pour JCL et COBOL

Uses the legacylens-jcl-parser library for proper AST-based parsing.
"""

import os
import sys
import argparse
from pathlib import Path
from collections import defaultdict

# Force UTF-8 output on Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

try:
    import graphviz
    GRAPHVIZ_AVAILABLE = True
except ImportError:
    GRAPHVIZ_AVAILABLE = False

# Try to import the proper JCL parser
try:
    from jcl_parser import JCLParser
    JCL_PARSER_AVAILABLE = True
except ImportError:
    try:
        # Alternative import path for legacylens package
        from legacylens_jcl_parser import JCLParser
        JCL_PARSER_AVAILABLE = True
    except ImportError:
        JCL_PARSER_AVAILABLE = False


def extract_pgm_from_jcl_ast(jcl_file_path):
    """
    Extract all PGM= values from JCL using the proper AST parser.
    
    Args:
        jcl_file_path: Path to the JCL file
        
    Returns:
        List of program names referenced in EXEC PGM= statements
    """
    if not JCL_PARSER_AVAILABLE:
        # Fallback to regex if parser not available
        return extract_pgm_from_jcl_regex(jcl_file_path)
    
    try:
        parser = JCLParser()
        
        with open(jcl_file_path, 'r', encoding='utf-8', errors='ignore') as f:
            jcl_content = f.read()
        
        parsed_jcl = parser.parse_string(jcl_content)
        
        # Extract programs from the parsed JCL structure
        programs = set()
        
        # The parser returns a dict with 'steps' containing EXEC statements
        if isinstance(parsed_jcl, dict):
            steps = parsed_jcl.get('steps', [])
            for step in steps:
                if isinstance(step, dict):
                    params = step.get('parameters', {})
                    if isinstance(params, dict):
                        pgm = params.get('PGM') or params.get('pgm')
                        if pgm:
                            # Clean up: remove any trailing parameters (e.g., "CBSTM03A,COND=(0,NE)")
                            pgm_clean = pgm.split(',')[0].strip().upper()
                            programs.add(pgm_clean)
        
        return list(programs) if programs else extract_pgm_from_jcl_regex(jcl_file_path)
        
    except Exception as e:
        # Log error and fallback to regex
        print(f"[WARN] AST parsing failed for {jcl_file_path}: {e}", file=sys.stderr)
        return extract_pgm_from_jcl_regex(jcl_file_path)


def extract_pgm_from_jcl_regex(jcl_file_path):
    """
    Fallback: Extract PGM= values using regex (when AST parser unavailable).
    """
    import re
    
    with open(jcl_file_path, 'r', encoding='utf-8', errors='ignore') as f:
        jcl_content = f.read()
    
    pattern = r'PGM=([A-Z0-9]+)'
    matches = re.findall(pattern, jcl_content, re.IGNORECASE)
    return list(set(m.upper() for m in matches))


def is_system_program(prog_name):
    """Check if program is a system program"""
    system_programs = {
        'IEFBR14', 'IDCAMS', 'SORT', 'SDSF', 'IEBGENER', 
        'IKJEFT01', 'IKJEFT1B', 'FTP', 'DFHCSDUP'
    }
    return prog_name in system_programs


def find_jcl_files(root_dir):
    """Recursively find all JCL files in directory tree"""
    jcl_files = []
    for path in Path(root_dir).rglob('*'):
        if path.is_file() and path.suffix.upper() in ['.JCL']:
            jcl_files.append(path)
    return sorted(jcl_files)


def find_cobol_files(root_dir):
    """Recursively find all COBOL files in directory tree and index by name"""
    cobol_index = {}  # { 'PROGNAME': Path }
    for path in Path(root_dir).rglob('*'):
        if path.is_file() and path.suffix.upper() in ['.CBL', '.COBOL', '.COB']:
            prog_name = path.stem.upper()  # Get filename without extension
            cobol_index[prog_name] = path
    return cobol_index


def main():
    parser = argparse.ArgumentParser(
        description='Analyze JCL → COBOL relationships (recursive)'
    )
    parser.add_argument(
        '-j', '--jcl-dir',
        required=True,
        help='Root directory to scan for JCL files (recursive)'
    )
    parser.add_argument(
        '-c', '--cobol-dir',
        required=True,
        help='Root directory to scan for COBOL files (recursive)'
    )
    parser.add_argument(
        '-o', '--output',
        default='out/report/JCL_to_COBOL_mapping',
        help='Output file path for SVG graph (default: out/report/JCL_to_COBOL_mapping)'
    )
    parser.add_argument(
        '--json',
        action='store_true',
        help='Output JSON format instead of SVG graph'
    )
    
    args = parser.parse_args()
    
    jcl_root = Path(args.jcl_dir)
    cbl_root = Path(args.cobol_dir)
    output_file = args.output
    
    # Validate directories exist
    if not jcl_root.exists():
        print(f"[ERROR] JCL directory not found: {jcl_root}", file=sys.stderr)
        sys.exit(1)
    if not cbl_root.exists():
        print(f"[ERROR] COBOL directory not found: {cbl_root}", file=sys.stderr)
        sys.exit(1)
    
    # Only print diagnostic info if NOT outputting JSON
    if not args.json:
        print(f"[SCAN] Analyzing JCL → COBOL mappings")
        print(f"       JCL root: {jcl_root}")
        print(f"       COBOL root: {cbl_root}")
        if JCL_PARSER_AVAILABLE:
            print(f"       Parser: legacylens-jcl-parser (AST-based)")
        else:
            print(f"       Parser: regex fallback (install jcl-parser for AST support)")
        print()
    
    # Find all JCL and COBOL files recursively
    if not args.json:
        print("[INDEX] Building COBOL index...")
    cobol_index = find_cobol_files(cbl_root)
    if not args.json:
        print(f"        Found {len(cobol_index)} COBOL files")
    
    if not args.json:
        print("[SCAN] Finding JCL files...")
    jcl_files = find_jcl_files(jcl_root)
    if not args.json:
        print(f"       Found {len(jcl_files)} JCL files")
        print()
    
    # Data structures
    jcl_to_cobol = defaultdict(list)  # JCL → [COBOL programs]
    cobol_to_jcl = defaultdict(list)  # COBOL → [JCL files]
    stats = {'found': 0, 'system': 0, 'missing': 0}
    
    # Scan JCL files
    for jcl_file in jcl_files:
        # Use AST-based extraction (with regex fallback)
        programs = extract_pgm_from_jcl_ast(jcl_file)
        
        if not programs:
            continue
        
        # Get relative path for display
        try:
            jcl_name = jcl_file.relative_to(jcl_root)
        except ValueError:
            jcl_name = jcl_file.name
        
        if not args.json:
            print(f"[JCL] {jcl_name}")
        
        for prog in programs:
            prog_upper = prog.upper()
            
            if prog_upper in cobol_index:
                cbl_file = cobol_index[prog_upper]
                if not args.json:
                    print(f"  ✅ {cbl_file.name}")
                jcl_to_cobol[str(jcl_name)].append(prog_upper)
                cobol_to_jcl[prog_upper].append(str(jcl_name))
                stats['found'] += 1
            elif is_system_program(prog_upper):
                if not args.json:
                    print(f"  ⚙️  {prog_upper} (system)")
                stats['system'] += 1
            else:
                if not args.json:
                    print(f"  ❌ {prog_upper}")
                stats['missing'] += 1
        
        if not args.json:
            print()
    
    # Output as JSON if requested
    if args.json:
        import json
        output_data = {
            "statistics": stats,
            "cbl_files": [
                {
                    "program": prog,
                    "jcl_files": cobol_to_jcl[prog]
                }
                for prog in sorted(cobol_to_jcl.keys())
            ]
        }
        print(json.dumps(output_data, indent=2))
        return
    
    # Print summary only if not JSON output
    if not args.json:
        print("=" * 60)
        print("SUMMARY: JCL → COBOL")
        print("=" * 60)
        print(f"Programs found: {stats['found']}")
        print(f"System programs: {stats['system']}")
        print(f"Missing programs: {stats['missing']}")
        print()
        print("COBOL Programs with JCL associations:")
        print()
        
        for prog in sorted(cobol_to_jcl.keys()):
            jcl_list = ', '.join(cobol_to_jcl[prog])
            print(f"  {prog}.cbl ← {jcl_list}")
        
        # Generate graph
        print()
        print("[GRAPH] Generating dependency graph...")
    
    if not args.json:
        if not GRAPHVIZ_AVAILABLE:
            print("[WARNING] graphviz not installed, skipping graph generation")
            print("          Install with: pip install graphviz")
            return
        
        graph = graphviz.Digraph(
            name='JCL_to_COBOL',
            comment='JCL to COBOL Dependency Mapping',
            format='svg',
            engine='dot'
        )
        
        graph.attr(rankdir='LR')
        graph.attr('node', shape='box', style='rounded,filled', fontname='Arial', fontsize='10')
        
        # Add JCL nodes
        with graph.subgraph(name='cluster_jcl') as jcl_cluster:
            jcl_cluster.attr(label='JCL Files', style='filled', color='lightblue')
            jcl_cluster.attr('node', fillcolor='#AADDFF', shape='note')
            for jcl in sorted(jcl_to_cobol.keys()):
                jcl_cluster.node(f"JCL_{jcl}", jcl.replace('.jcl', '').replace('.JCL', ''))
        
        # Add COBOL nodes
        with graph.subgraph(name='cluster_cobol') as cbl_cluster:
            cbl_cluster.attr(label='COBOL Programs', style='filled', color='lightgreen')
            cbl_cluster.attr('node', fillcolor='#AAFFAA', shape='component')
            for prog in sorted(cobol_to_jcl.keys()):
                cbl_cluster.node(f"CBL_{prog}", prog)
        
        # Add edges
        for jcl, programs in jcl_to_cobol.items():
            jcl_node = f"JCL_{jcl}"
            for prog in programs:
                cbl_node = f"CBL_{prog}"
                graph.edge(jcl_node, cbl_node, label='executes')
        
        # Render
        output_path = Path(output_file)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        graph.render(str(output_path), cleanup=True)
        
        print(f"[OK] Graph generated: {output_path}.svg")

if __name__ == '__main__':
    main()
