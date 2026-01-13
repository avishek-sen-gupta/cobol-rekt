#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Analyse JCL → COBOL: Partir des JCL pour tracer les fichiers COBOL
GÉNÉRIQUE: Scanne récursivement tous les répertoires pour JCL et COBOL

Uses the centralized JCL analyzer for AST-based parsing.
"""

import os
import sys
import argparse
from pathlib import Path

# Force UTF-8 output on Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

# Add the jcl parser path
SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent
JCL_PARSER_PATH = PROJECT_ROOT / "smojol-jcl" / "python"
sys.path.insert(0, str(JCL_PARSER_PATH))

try:
    import graphviz
    GRAPHVIZ_AVAILABLE = True
except ImportError:
    GRAPHVIZ_AVAILABLE = False

# Import the centralized JCL analyzer
try:
    from jcl_analysis import JCLAnalyzer, SYSTEM_PROGRAMS
    JCL_ANALYZER_AVAILABLE = True
except ImportError:
    JCL_ANALYZER_AVAILABLE = False
    SYSTEM_PROGRAMS = {
        'IEFBR14', 'IDCAMS', 'SORT', 'SDSF', 'IEBGENER',
        'IKJEFT01', 'IKJEFT1B', 'FTP', 'DFHCSDUP'
    }


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
    
    # Use centralized analyzer
    if JCL_ANALYZER_AVAILABLE:
        analyzer = JCLAnalyzer(verbose=not args.json)
        
        if not args.json:
            print(f"[SCAN] Analyzing JCL → COBOL mappings")
            print(f"       JCL root: {jcl_root}")
            print(f"       COBOL root: {cbl_root}")
            print(f"       Parser: {'legacylens-jcl-parser (AST-based)' if analyzer.parser_available else 'regex fallback'}")
            print()
        
        # Load files
        if not args.json:
            print("[INDEX] Building COBOL index...")
        cbl_count = analyzer.load_cobol_index(cbl_root)
        if not args.json:
            print(f"        Found {cbl_count} COBOL files")
            print("[SCAN] Finding JCL files...")
        
        jcl_count = analyzer.load_jcl_directory(jcl_root)
        if not args.json:
            print(f"       Found {jcl_count} JCL files")
            print()
        
        # Get mappings
        jcl_to_cobol = analyzer.jcl_to_cobol
        cobol_to_jcl = analyzer.cobol_to_jcl
        stats = analyzer.get_mapping_stats()
        
        # Verbose output
        if not args.json:
            for jcl_key, parsed in sorted(analyzer.jcl_files.items()):
                print(f"[JCL] {parsed.name}")
                for prog in parsed.programs:
                    prog_upper = prog.upper()
                    if prog_upper in analyzer.cobol_index:
                        cbl_file = analyzer.cobol_index[prog_upper]
                        print(f"  ✅ {cbl_file.name}")
                    elif analyzer.is_system_program(prog_upper):
                        print(f"  ⚙️  {prog_upper} (system)")
                    else:
                        print(f"  ❌ {prog_upper}")
                print()
        
        # Output as JSON if requested
        if args.json:
            import json
            output_data = analyzer.get_cobol_mapping_json()
            print(json.dumps(output_data, indent=2))
            return
        
        # Print summary
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
    
    else:
        # Fallback if analyzer not available
        print("[ERROR] JCL analyzer not available. Install jcl-parser or check the module path.", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
