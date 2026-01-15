#!/usr/bin/env python3
"""
JCL Analysis Module
Centralized JCL parsing and analysis using the legacylens-jcl-parser library.

This module provides:
- Single-pass JCL parsing for all analysis needs
- Program extraction (PGM=)
- Dataset extraction (DD statements)
- Step extraction
- JCL-to-COBOL mapping

Usage:
    from jcl_analysis import JCLAnalyzer
    
    analyzer = JCLAnalyzer()
    analyzer.load_jcl_directory('/path/to/jcl')
    analyzer.load_cobol_index('/path/to/cobol')
    
    # Get all analysis results
    results = analyzer.get_full_analysis()
"""

import os
import sys
import json
import re
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
from collections import defaultdict

# Try to import the proper JCL parser
try:
    from jcl_parser import JCLParser
    JCL_PARSER_AVAILABLE = True
except ImportError:
    JCL_PARSER_AVAILABLE = False

# Import preprocessing utilities
try:
    from jcl_preprocessing import preprocess_jcl_continuations, fix_jcl_parameters_in_result
except ImportError:
    # Fallback if not available (shouldn't happen in normal usage)
    def preprocess_jcl_continuations(content):
        return content
    def fix_jcl_parameters_in_result(result):
        return result


# System programs that should be excluded from COBOL mapping
SYSTEM_PROGRAMS = {
    'IEFBR14', 'IDCAMS', 'SORT', 'SDSF', 'IEBGENER',
    'IKJEFT01', 'IKJEFT1B', 'FTP', 'DFHCSDUP', 'DFSORT',
    'ICETOOL', 'IEBCOPY', 'IEBUPDTE', 'ADRDSSU', 'DSNUPROC'
}


class ParsedJCL:
    """Represents a parsed JCL file with all extracted information."""
    
    def __init__(self, path: Path):
        self.path = path
        self.name = path.name
        self.stem = path.stem
        self.raw_data: Optional[Dict[str, Any]] = None
        self.parse_error: Optional[str] = None
        
        # Extracted data (cached after first access)
        self._programs: Optional[List[str]] = None
        self._dd_names: Optional[List[Dict[str, Any]]] = None
        self._steps: Optional[List[Dict[str, Any]]] = None
        self._job_name: Optional[str] = None
        self._lines: int = 0
        self._size: int = 0
    
    @property
    def is_parsed(self) -> bool:
        return self.raw_data is not None
    
    @property
    def job_name(self) -> str:
        if self._job_name is None:
            self._job_name = ''
            if self.raw_data and 'job' in self.raw_data:
                self._job_name = self.raw_data['job'].get('name', '')
        return self._job_name
    
    @property
    def programs(self) -> List[str]:
        """Extract all PGM= values from parsed JCL."""
        if self._programs is None:
            self._programs = []
            if self.raw_data:
                steps = self.raw_data.get('steps', [])
                seen = set()
                for step in steps:
                    if isinstance(step, dict):
                        params = step.get('parameters', {})
                        if isinstance(params, dict):
                            pgm = params.get('PGM') or params.get('pgm')
                            if pgm:
                                # Clean: remove trailing parameters
                                pgm_clean = pgm.split(',')[0].strip().upper()
                                if pgm_clean and pgm_clean not in seen:
                                    seen.add(pgm_clean)
                                    self._programs.append(pgm_clean)
        return self._programs
    
    @property
    def dd_names(self) -> List[Dict[str, Any]]:
        """Extract DD (Data Definition) names from parsed JCL."""
        if self._dd_names is None:
            self._dd_names = []
            if self.raw_data:
                seen = set()
                steps = self.raw_data.get('steps', [])
                for step in steps:
                    dd_statements = step.get('dd_statements', [])
                    for dd in dd_statements:
                        dd_name = dd.get('name', '')
                        if dd_name and dd_name not in seen:
                            seen.add(dd_name)
                            self._dd_names.append({
                                'name': dd_name,
                                'step': step.get('name', ''),
                                'line': dd.get('line', 0)
                            })
        return self._dd_names
    
    @property
    def steps(self) -> List[Dict[str, Any]]:
        """Extract step information from parsed JCL."""
        if self._steps is None:
            self._steps = []
            if self.raw_data:
                for step in self.raw_data.get('steps', []):
                    params = step.get('parameters', {})
                    pgm = ''
                    if isinstance(params, dict):
                        pgm = params.get('PGM') or params.get('pgm') or ''
                        if pgm:
                            pgm = pgm.split(',')[0].strip()
                    
                    self._steps.append({
                        'name': step.get('name', ''),
                        'program': pgm,
                        'line': step.get('line', 0),
                        'dd_count': len(step.get('dd_statements', []))
                    })
        return self._steps
    
    def _extract_datasets(self) -> Dict[str, List[Dict[str, Any]]]:
        """Extract and categorize datasets (INPUT, OUTPUT, SYSOUT)."""
        datasets = {
            'INPUT': [],
            'OUTPUT': [],
            'SYSOUT': []
        }
        
        if not self.raw_data:
            return datasets
        
        for step in self.raw_data.get('steps', []):
            for dd in step.get('dd_statements', []):
                dd_name = dd.get('name', '')
                params = dd.get('parameters', {})
                
                if not isinstance(params, dict):
                    continue
                
                # SYSOUT
                if 'SYSOUT' in params:
                    datasets['SYSOUT'].append({
                        'dd_name': dd_name,
                        'destination': params['SYSOUT']
                    })
                    continue
                
                # Datasets with DSN
                dsn = params.get('DSN', '')
                if dsn:
                    disp_str = params.get('DISP', '')
                    
                    # Parse disposition to determine INPUT/OUTPUT
                    disposition = 'SHR'  # Default
                    if disp_str:
                        # Remove parentheses if present
                        disp_clean = disp_str.strip('()')
                        disposition = disp_clean.split(',')[0].upper()
                    
                    dataset_entry = {
                        'dd_name': dd_name,
                        'dsn': dsn,
                        'disposition': disposition
                    }
                    
                    # Add additional info for OUTPUT datasets
                    if disposition in ['NEW', 'MOD']:
                        if 'SPACE' in params:
                            dataset_entry['space'] = params['SPACE']
                        
                        if 'DCB' in params:
                            dcb_str = params['DCB'].strip('()')
                            dcb_parts = {}
                            for part in dcb_str.split(','):
                                if '=' in part:
                                    k, v = part.split('=', 1)
                                    dcb_parts[k.lower()] = v
                            if dcb_parts:
                                dataset_entry['dcb'] = dcb_parts
                        
                        datasets['OUTPUT'].append(dataset_entry)
                    else:
                        # SHR, OLD = INPUT
                        datasets['INPUT'].append(dataset_entry)
        
        return datasets
    
    def _enrich_dd_statement(self, dd: Dict[str, Any]) -> Dict[str, Any]:
        """Enrich a DD statement with parsed dataset information."""
        enriched = {
            'name': dd.get('name', ''),
            'line': dd.get('line', 0),
            'parameters': dd.get('parameters', {})
        }
        
        params = dd.get('parameters', {})
        if isinstance(params, dict):
            # Add dataset_info if DSN is present
            dsn = params.get('DSN', '')
            if dsn:
                disp_str = params.get('DISP', '')
                disposition = 'SHR'
                dataset_type = 'INPUT'
                
                if disp_str:
                    disp_clean = disp_str.strip('()')
                    disposition = disp_clean.split(',')[0].upper()
                    if disposition in ['NEW', 'MOD']:
                        dataset_type = 'OUTPUT'
                
                dataset_info = {
                    'dsn': dsn,
                    'disposition': disposition,
                    'type': dataset_type
                }
                
                # Extract DCB information
                dcb_str = params.get('DCB', '')
                if dcb_str:
                    dcb_clean = dcb_str.strip('()')
                    for part in dcb_clean.split(','):
                        if '=' in part:
                            k, v = part.split('=', 1)
                            k_lower = k.lower()
                            if k_lower == 'recfm':
                                dataset_info['recordFormat'] = v
                            elif k_lower == 'lrecl':
                                try:
                                    dataset_info['recordLength'] = int(v)
                                except ValueError:
                                    dataset_info['recordLength'] = v
                            elif k_lower == 'blksize':
                                try:
                                    dataset_info['blockSize'] = int(v)
                                except ValueError:
                                    dataset_info['blockSize'] = v
                
                enriched['dataset_info'] = dataset_info
        
        return enriched
    
    def to_dict(self, include_details: bool = False) -> Dict[str, Any]:
        """
        Convert to dictionary for JSON serialization.
        
        Args:
            include_details: If True, include detailed DD statement parameters
        """
        result = {
            'name': self.name,
            'stem': self.stem,
            'path': str(self.path),
            'job_name': self.job_name,
            'programs': self.programs,
            'dd_names': [d['name'] for d in self.dd_names],
            'steps': self.steps if not include_details else self._get_detailed_steps(),
            'lines': self._lines,
            'size': self._size,
            'parse_error': self.parse_error
        }
        
        if include_details:
            result['datasets'] = self._extract_datasets()
        
        return result
    
    def _get_detailed_steps(self) -> List[Dict[str, Any]]:
        """Get steps with detailed DD statement information."""
        if not self.raw_data:
            return []
        
        detailed_steps = []
        for step in self.raw_data.get('steps', []):
            params = step.get('parameters', {})
            pgm = ''
            if isinstance(params, dict):
                pgm = params.get('PGM') or params.get('pgm') or ''
                if pgm:
                    pgm = pgm.split(',')[0].strip()
            
            dd_statements = [
                self._enrich_dd_statement(dd)
                for dd in step.get('dd_statements', [])
            ]
            
            detailed_steps.append({
                'name': step.get('name', ''),
                'program': pgm,
                'line': step.get('line', 0),
                'dd_count': len(dd_statements),
                'dd_statements': dd_statements
            })
        
        return detailed_steps


class JCLAnalyzer:
    """
    Centralized JCL analysis.
    Parses JCL files once and provides all analysis views.
    """
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.parser_available = JCL_PARSER_AVAILABLE
        self._parser: Optional[JCLParser] = None
        
        # Parsed JCL files
        self.jcl_files: Dict[str, ParsedJCL] = {}
        
        # COBOL index
        self.cobol_index: Dict[str, Path] = {}
        
        # Computed mappings (lazy)
        self._jcl_to_cobol: Optional[Dict[str, List[str]]] = None
        self._cobol_to_jcl: Optional[Dict[str, List[str]]] = None
        self._all_dd_names: Optional[Dict[str, Dict[str, Any]]] = None
    
    @property
    def parser(self) -> Optional[JCLParser]:
        """Lazy-load the JCL parser."""
        if self._parser is None and self.parser_available:
            self._parser = JCLParser()
        return self._parser
    
    def _log(self, message: str):
        """Log message if verbose mode is enabled."""
        if self.verbose:
            print(message, file=sys.stderr)
    
    def parse_jcl_file(self, jcl_path: Path) -> ParsedJCL:
        """Parse a single JCL file."""
        parsed = ParsedJCL(jcl_path)
        
        # Get file stats
        try:
            stat = jcl_path.stat()
            parsed._size = stat.st_size
            with open(jcl_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                parsed._lines = content.count('\n') + 1
        except Exception as e:
            parsed.parse_error = f"Failed to read file: {e}"
            return parsed
        
        # Preprocess JCL to handle multi-line continuations
        content = preprocess_jcl_continuations(content)
        
        # Parse with AST parser if available
        if self.parser:
            try:
                result = self.parser.parse_string(content)
                
                # Handle different return types
                if hasattr(result, 'to_json'):
                    parsed.raw_data = result.to_json()
                elif hasattr(result, '__dict__'):
                    parsed.raw_data = result.__dict__
                elif isinstance(result, dict):
                    parsed.raw_data = result
                else:
                    parsed.raw_data = {'steps': []}
                
                # Fix parameter parsing (generic solution for parentheses, quotes, etc.)
                if parsed.raw_data:
                    parsed.raw_data = fix_jcl_parameters_in_result(parsed.raw_data)
                    
            except Exception as e:
                self._log(f"[WARN] AST parsing failed for {jcl_path.name}: {e}")
                parsed.parse_error = str(e)
                # Fall back to regex
                parsed.raw_data = self._parse_jcl_regex(content)
        else:
            # Use regex fallback
            parsed.raw_data = self._parse_jcl_regex(content)
        
        return parsed
    
    def _parse_jcl_regex(self, content: str) -> Dict[str, Any]:
        """Fallback regex-based JCL parsing."""
        programs = []
        pattern = r'PGM=([A-Z0-9]+)'
        matches = re.findall(pattern, content, re.IGNORECASE)
        programs = list(set(m.upper() for m in matches))
        
        # Create minimal structure
        steps = [{'parameters': {'PGM': p}} for p in programs]
        return {'steps': steps}
    
    def load_jcl_directory(self, jcl_dir: Path) -> int:
        """
        Load and parse all JCL files in a directory.
        
        Returns:
            Number of JCL files loaded
        """
        jcl_dir = Path(jcl_dir)
        if not jcl_dir.exists():
            raise ValueError(f"JCL directory not found: {jcl_dir}")
        
        # Find all JCL files
        jcl_paths = list(jcl_dir.glob('**/*.jcl')) + list(jcl_dir.glob('**/*.JCL'))
        
        for jcl_path in sorted(jcl_paths):
            self._log(f"  Parsing: {jcl_path.name}")
            parsed = self.parse_jcl_file(jcl_path)
            self.jcl_files[jcl_path.stem.upper()] = parsed
        
        # Reset computed mappings
        self._jcl_to_cobol = None
        self._cobol_to_jcl = None
        self._all_dd_names = None
        
        return len(self.jcl_files)
    
    def load_cobol_index(self, cobol_dir: Path) -> int:
        """
        Build index of COBOL files in a directory.
        
        Returns:
            Number of COBOL files indexed
        """
        cobol_dir = Path(cobol_dir)
        if not cobol_dir.exists():
            raise ValueError(f"COBOL directory not found: {cobol_dir}")
        
        self.cobol_index = {}
        for path in cobol_dir.rglob('*'):
            if path.is_file() and path.suffix.upper() in ['.CBL', '.COBOL', '.COB']:
                prog_name = path.stem.upper()
                self.cobol_index[prog_name] = path
        
        # Reset computed mappings
        self._jcl_to_cobol = None
        self._cobol_to_jcl = None
        
        return len(self.cobol_index)
    
    def _compute_mappings(self):
        """Compute JCL-COBOL mappings (lazy computation)."""
        if self._jcl_to_cobol is not None:
            return
        
        self._jcl_to_cobol = defaultdict(list)
        self._cobol_to_jcl = defaultdict(list)
        
        for jcl_key, parsed in self.jcl_files.items():
            jcl_name = parsed.stem
            for prog in parsed.programs:
                prog_upper = prog.upper()
                if prog_upper in self.cobol_index:
                    if prog_upper not in self._jcl_to_cobol[jcl_name]:
                        self._jcl_to_cobol[jcl_name].append(prog_upper)
                    if jcl_name not in self._cobol_to_jcl[prog_upper]:
                        self._cobol_to_jcl[prog_upper].append(jcl_name)
    
    def _compute_dd_names(self):
        """Compute aggregated DD name information (lazy computation)."""
        if self._all_dd_names is not None:
            return
        
        self._all_dd_names = {}
        for jcl_key, parsed in self.jcl_files.items():
            for dd in parsed.dd_names:
                dd_name = dd['name']
                if dd_name not in self._all_dd_names:
                    self._all_dd_names[dd_name] = {
                        'name': dd_name,
                        'jcl_files': [],
                        'type': 'unknown'
                    }
                if parsed.stem not in self._all_dd_names[dd_name]['jcl_files']:
                    self._all_dd_names[dd_name]['jcl_files'].append(parsed.stem)
    
    @property
    def jcl_to_cobol(self) -> Dict[str, List[str]]:
        """Get JCL -> COBOL program mappings."""
        self._compute_mappings()
        return dict(self._jcl_to_cobol)
    
    @property
    def cobol_to_jcl(self) -> Dict[str, List[str]]:
        """Get COBOL program -> JCL mappings."""
        self._compute_mappings()
        return dict(self._cobol_to_jcl)
    
    @property
    def all_dd_names(self) -> Dict[str, Dict[str, Any]]:
        """Get all DD names across all JCL files."""
        self._compute_dd_names()
        return self._all_dd_names
    
    def get_mapping_stats(self) -> Dict[str, int]:
        """Get statistics about JCL-COBOL mappings."""
        self._compute_mappings()
        
        found = 0
        system = 0
        missing = 0
        
        for parsed in self.jcl_files.values():
            for prog in parsed.programs:
                prog_upper = prog.upper()
                if prog_upper in self.cobol_index:
                    found += 1
                elif prog_upper in SYSTEM_PROGRAMS:
                    system += 1
                else:
                    missing += 1
        
        return {
            'found': found,
            'system': system,
            'missing': missing
        }
    
    def is_system_program(self, prog_name: str) -> bool:
        """Check if a program is a system program."""
        return prog_name.upper() in SYSTEM_PROGRAMS
    
    # ========================================================================
    # Output formats for different consumers
    # ========================================================================
    
    def get_jcl_analysis_json(self, include_details: bool = True) -> Dict[str, Any]:
        """
        Get JCL analysis in the format expected by smojol-ui.
        Used by generate_ui_json.py.
        
        Args:
            include_details: If True, include detailed DD parameters and dataset info
        """
        self._compute_dd_names()
        
        jcl_list = []
        total_dd_statements = 0
        total_datasets = 0
        dataset_summary = {'INPUT': 0, 'OUTPUT': 0, 'SYSOUT': 0}
        
        for parsed in sorted(self.jcl_files.values(), key=lambda p: p.name):
            jcl_dict = parsed.to_dict(include_details=include_details)
            jcl_list.append(jcl_dict)
            
            if include_details:
                # Count DD statements
                for step in jcl_dict.get('steps', []):
                    total_dd_statements += len(step.get('dd_statements', []))
                
                # Count datasets
                datasets = jcl_dict.get('datasets', {})
                for dataset_type, ds_list in datasets.items():
                    count = len(ds_list)
                    total_datasets += count
                    if dataset_type in dataset_summary:
                        dataset_summary[dataset_type] += count
        
        # Enrich summary with DD name type information
        dd_names_enriched = []
        for dd_name, dd_info in self._all_dd_names.items():
            # Determine type by checking actual usage across JCL files
            dd_type = 'unknown'
            count = len(dd_info['jcl_files'])
            
            # Check first occurrence to determine type
            for parsed in self.jcl_files.values():
                if not parsed.raw_data:
                    continue
                for step in parsed.raw_data.get('steps', []):
                    for dd in step.get('dd_statements', []):
                        if dd.get('name') == dd_name:
                            params = dd.get('parameters', {})
                            if isinstance(params, dict):
                                if 'SYSOUT' in params:
                                    dd_type = 'SYSOUT'
                                elif 'DSN' in params:
                                    disp = params.get('DISP', 'SHR')
                                    if disp and disp.strip('()').split(',')[0].upper() in ['NEW', 'MOD']:
                                        dd_type = 'OUTPUT'
                                    else:
                                        dd_type = 'INPUT'
                            break
                    if dd_type != 'unknown':
                        break
                if dd_type != 'unknown':
                    break
            
            dd_names_enriched.append({
                'name': dd_name,
                'jcl_files': dd_info['jcl_files'],
                'type': dd_type,
                'count': count
            })
        
        summary = {
            'total_jcl_files': len(jcl_list),
            'dd_names': dd_names_enriched
        }
        
        if include_details:
            summary['total_programs'] = sum(len(p.programs) for p in self.jcl_files.values())
            summary['total_dd_statements'] = total_dd_statements
            summary['total_datasets'] = total_datasets
            summary['dataset_summary'] = dataset_summary
        
        return {
            'jcl_files': jcl_list,
            'summary': summary
        }
    
    def get_cobol_mapping_json(self) -> Dict[str, Any]:
        """
        Get COBOL-JCL mappings in the format expected by scan_and_analyze_project.sh.
        Used by analyze_jcl_to_cobol.py.
        """
        stats = self.get_mapping_stats()
        
        return {
            'statistics': stats,
            'cbl_files': [
                {
                    'program': prog,
                    'jcl_files': jcl_list
                }
                for prog, jcl_list in sorted(self.cobol_to_jcl.items())
            ]
        }
    
    def get_full_analysis(self) -> Dict[str, Any]:
        """
        Get complete analysis including all data.
        """
        return {
            'jcl_analysis': self.get_jcl_analysis_json(),
            'cobol_mapping': self.get_cobol_mapping_json(),
            'parser_used': 'ast' if self.parser_available else 'regex'
        }


# ============================================================================
# CLI entry point
# ============================================================================

def main():
    """CLI entry point for JCL analysis."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Analyze JCL files and generate mappings'
    )
    parser.add_argument(
        '-j', '--jcl-dir',
        type=Path,
        required=True,
        help='Directory containing JCL files'
    )
    parser.add_argument(
        '-c', '--cobol-dir',
        type=Path,
        help='Directory containing COBOL files (for mapping)'
    )
    parser.add_argument(
        '-o', '--output',
        type=Path,
        help='Output JSON file'
    )
    parser.add_argument(
        '--format',
        choices=['full', 'jcl', 'mapping'],
        default='full',
        help='Output format (default: full)'
    )
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Verbose output'
    )
    
    args = parser.parse_args()
    
    analyzer = JCLAnalyzer(verbose=args.verbose)
    
    # Load JCL files
    if args.verbose:
        print(f"Loading JCL files from {args.jcl_dir}...", file=sys.stderr)
    jcl_count = analyzer.load_jcl_directory(args.jcl_dir)
    if args.verbose:
        print(f"  Loaded {jcl_count} JCL files", file=sys.stderr)
    
    # Load COBOL index if provided
    if args.cobol_dir:
        if args.verbose:
            print(f"Loading COBOL files from {args.cobol_dir}...", file=sys.stderr)
        cbl_count = analyzer.load_cobol_index(args.cobol_dir)
        if args.verbose:
            print(f"  Indexed {cbl_count} COBOL files", file=sys.stderr)
    
    # Generate output
    if args.format == 'jcl':
        result = analyzer.get_jcl_analysis_json()
    elif args.format == 'mapping':
        result = analyzer.get_cobol_mapping_json()
    else:
        result = analyzer.get_full_analysis()
    
    # Output
    output_json = json.dumps(result, indent=2)
    
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(output_json)
        if args.verbose:
            print(f"Written: {args.output}", file=sys.stderr)
    else:
        print(output_json)


if __name__ == '__main__':
    main()
