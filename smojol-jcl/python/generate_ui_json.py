#!/usr/bin/env python3
"""
Generate UI JSON files for smojol-ui
Uses the centralized JCL analyzer and aggregated COBOL ASTs to generate:
- jcl-analysis.json: JCL files with jobs, steps, programs, datasets
- copybook-analysis-complete.json: Programs with their copybook usage

Usage:
    python generate_ui_json.py -j <jcl_dir> -r <report_dir> -o <output_dir>
"""

import os
import sys
import json
import argparse
from pathlib import Path
from typing import Dict, List, Any, Optional

# Add the jcl parser path
SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent

# Import the centralized JCL analyzer
try:
    from jcl_analysis import JCLAnalyzer
    JCL_ANALYZER_AVAILABLE = True
except ImportError:
    JCL_ANALYZER_AVAILABLE = False
    print("[WARN] jcl_analysis module not available, JCL parsing will be skipped", file=sys.stderr)


def load_aggregated_ast(ast_path: Path) -> Optional[Dict[str, Any]]:
    """Load an aggregated AST JSON file."""
    try:
        with open(ast_path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except Exception as e:
        print(f"[WARN] Failed to load {ast_path}: {e}", file=sys.stderr)
        return None


def extract_copybooks_from_ast(ast_data: Dict[str, Any]) -> Dict[str, Any]:
    """Extract copybook information from aggregated AST."""
    result = {
        'copybooks': [],
        'copybooks_metadata': {}
    }
    
    if not ast_data:
        return result
    
    # Get copybook list
    copybooks = ast_data.get('copybooks', [])
    result['copybooks'] = copybooks
    
    # Get copybook metadata
    metadata = ast_data.get('copybooksMetadata', {})
    result['copybooks_metadata'] = metadata
    
    return result


def process_report_directory(report_dir: Path) -> Dict[str, Any]:
    """Process all aggregated ASTs in the report directory."""
    programs = []
    all_copybooks = {}
    
    # Find all aggregated JSON files
    ast_files = list(report_dir.glob('**/*-aggregated.json'))
    
    for ast_path in sorted(ast_files):
        # Extract program name from path
        # Pattern: PROGRAM.cbl.report/ast/aggregated/PROGRAM-aggregated.json
        program_name = ast_path.stem.replace('-aggregated', '')
        
        print(f"  Processing: {program_name}", file=sys.stderr)
        
        ast_data = load_aggregated_ast(ast_path)
        copybook_info = extract_copybooks_from_ast(ast_data)
        
        # Build copybook list with metadata
        copybooks_with_meta = []
        for cpy_name in copybook_info['copybooks']:
            meta = copybook_info['copybooks_metadata'].get(cpy_name, {})
            
            # Track global copybook info
            if cpy_name not in all_copybooks:
                all_copybooks[cpy_name] = {
                    'name': cpy_name,
                    'uri': meta.get('uri', ''),
                    'size': meta.get('size', 0),
                    'lines': meta.get('lines', 0),
                    'used_by': [],
                    'exists': bool(meta.get('uri'))
                }
            
            if program_name not in all_copybooks[cpy_name]['used_by']:
                all_copybooks[cpy_name]['used_by'].append(program_name)
            
            copybooks_with_meta.append({
                'name': cpy_name,
                'uri': meta.get('uri', ''),
                'size': meta.get('size', 0),
                'lines': meta.get('lines', 0),
                'usages': meta.get('usages', []),
                'includes': meta.get('includes', []),
                'exists': bool(meta.get('uri'))
            })
        
        # Find the CBL file path
        cbl_path = ''
        report_parent = ast_path.parent.parent.parent  # Go up from aggregated/ to *.report/
        if report_parent.name.endswith('.report'):
            cbl_name = report_parent.name.replace('.report', '')
            # The path would be in the original source location
            # For now, just use the report path structure
            cbl_path = str(report_parent)
        
        programs.append({
            'program': program_name,
            'path': cbl_path,
            'copybooks': copybooks_with_meta
        })
    
    return {
        'programs': programs,
        'copybooks': list(all_copybooks.values()),
        'summary': {
            'total_programs': len(programs),
            'total_copybooks': len(all_copybooks)
        }
    }


def main():
    parser = argparse.ArgumentParser(
        description='Generate UI JSON files from JCL analyzer and aggregated ASTs'
    )
    parser.add_argument(
        '-j', '--jcl-dir',
        type=Path,
        required=True,
        help='Directory containing JCL files'
    )
    parser.add_argument(
        '-r', '--report-dir',
        type=Path,
        required=True,
        help='Directory containing aggregated AST reports'
    )
    parser.add_argument(
        '-o', '--output-dir',
        type=Path,
        required=True,
        help='Output directory for JSON files'
    )
    
    args = parser.parse_args()
    
    # Validate directories
    if not args.jcl_dir.exists():
        print(f"ERROR: JCL directory not found: {args.jcl_dir}", file=sys.stderr)
        sys.exit(1)
    
    if not args.report_dir.exists():
        print(f"ERROR: Report directory not found: {args.report_dir}", file=sys.stderr)
        sys.exit(1)
    
    # Create output directory
    args.output_dir.mkdir(parents=True, exist_ok=True)
    
    # Process JCL files using centralized analyzer
    print("[Step 1/2] Processing JCL files...", file=sys.stderr)
    
    if JCL_ANALYZER_AVAILABLE:
        analyzer = JCLAnalyzer(verbose=True)
        jcl_count = analyzer.load_jcl_directory(args.jcl_dir)
        jcl_analysis = analyzer.get_jcl_analysis_json()
        print(f"  Total JCL files: {jcl_count}", file=sys.stderr)
    else:
        jcl_analysis = {'jcl_files': [], 'summary': {'total_jcl_files': 0, 'datasets': []}}
        print("  Skipped (analyzer not available)", file=sys.stderr)
    
    jcl_output = args.output_dir / 'jcl-analysis.json'
    with open(jcl_output, 'w', encoding='utf-8') as f:
        json.dump(jcl_analysis, f, indent=2)
    print(f"  Written: {jcl_output}", file=sys.stderr)
    
    # Process AST reports
    print("[Step 2/2] Processing aggregated ASTs...", file=sys.stderr)
    copybook_analysis = process_report_directory(args.report_dir)
    
    copybook_output = args.output_dir / 'copybook-analysis-complete.json'
    with open(copybook_output, 'w', encoding='utf-8') as f:
        json.dump(copybook_analysis, f, indent=2)
    print(f"  Written: {copybook_output}", file=sys.stderr)
    print(f"  Total programs: {copybook_analysis['summary']['total_programs']}", file=sys.stderr)
    print(f"  Total copybooks: {copybook_analysis['summary']['total_copybooks']}", file=sys.stderr)
    
    print("\nDone!", file=sys.stderr)


if __name__ == '__main__':
    main()
