"""
JCL Preprocessing utilities

This module provides generic preprocessing functions for JCL files,
including continuation line handling and parameter parsing.
"""

import re


def preprocess_jcl_continuations(jcl_content):
    """
    Preprocess JCL content to handle multi-line continuations.
    
    JCL continuation rules:
    - A statement is continued when the next line starts with '//' followed by spaces (typically 9+ spaces)
    - The continued line should be merged with the previous line
    - Comments (//*) should not be treated as continuations
    
    Args:
        jcl_content (str): The raw JCL content
        
    Returns:
        str: JCL content with continuations resolved
    """
    lines = jcl_content.split('\n')
    result_lines = []
    i = 0
    
    while i < len(lines):
        current_line = lines[i]
        
        # Skip empty lines and comments
        if not current_line.strip() or current_line.strip().startswith('//*'):
            result_lines.append(current_line)
            i += 1
            continue
        
        # Check if this is a JCL statement (starts with //)
        if current_line.startswith('//') and not current_line.startswith('//*'):
            # Accumulate continuation lines
            accumulated = current_line
            i += 1
            
            # Look for continuation lines
            while i < len(lines):
                next_line = lines[i]
                
                # Check if next line is a continuation (// followed by spaces)
                # Typically 9+ spaces, but we'll be lenient and accept 2+ spaces
                continuation_pattern = r'^//\s{2,}'
                
                if re.match(continuation_pattern, next_line) and not next_line.strip().startswith('//*'):
                    # This is a continuation line
                    # Remove the leading '//' and spaces, keep the rest
                    continuation_content = re.sub(r'^//\s+', '', next_line)
                    
                    # Merge with accumulated content
                    # Remove trailing comma from accumulated if present, we'll add it back
                    accumulated = accumulated.rstrip()
                    if not accumulated.endswith(','):
                        accumulated += ','
                    accumulated += continuation_content
                    i += 1
                else:
                    # Not a continuation, stop accumulating
                    break
            
            result_lines.append(accumulated)
        else:
            # Not a JCL statement, keep as is
            result_lines.append(current_line)
            i += 1
    
    return '\n'.join(result_lines)


def parse_jcl_parameters(value_string):
    """
    Parse JCL parameters from a value string, respecting parentheses, quotes, and nesting.
    
    This is a generic parser that handles:
    - Nested parentheses: DISP=(NEW,CATLG,DELETE)
    - Quoted values: LABEL=('FILE 001',SL)
    - Complex nesting: SPACE=(TRK,(50,25),RLSE)
    - Special values: DUMMY, *, *.STEP1.DD1
    
    Args:
        value_string (str): The parameter string from _value field (e.g., "DISP=(NEW,CATLG),DSN=...")
        
    Returns:
        dict: Dictionary of parsed parameters {KEY: VALUE}
    """
    if not value_string or not isinstance(value_string, str):
        return {}
    
    parameters = {}
    current_param = ""
    paren_depth = 0
    quote_char = None  # Track if we're inside quotes (' or ")
    i = 0
    
    while i < len(value_string):
        char = value_string[i]
        
        # Handle quotes
        if char in ('"', "'"):
            if quote_char is None:
                quote_char = char  # Entering quoted section
            elif char == quote_char:
                quote_char = None  # Exiting quoted section
            current_param += char
            
        # Handle parentheses (only when not in quotes)
        elif char == '(' and quote_char is None:
            paren_depth += 1
            current_param += char
            
        elif char == ')' and quote_char is None:
            paren_depth -= 1
            current_param += char
            
        # Handle comma separator (only at depth 0 and not in quotes)
        elif char == ',' and paren_depth == 0 and quote_char is None:
            # Process the accumulated parameter
            param = current_param.strip()
            if param:
                _add_parameter(parameters, param)
            current_param = ""
            
        else:
            current_param += char
        
        i += 1
    
    # Don't forget the last parameter
    param = current_param.strip()
    if param:
        _add_parameter(parameters, param)
    
    return parameters


def _add_parameter(params_dict, param_string):
    """
    Add a single parameter to the dictionary.
    Handles KEY=VALUE pairs and standalone values.
    
    Args:
        params_dict (dict): The parameters dictionary to add to
        param_string (str): A single parameter string (e.g., "DISP=(NEW,CATLG)" or "DUMMY")
    """
    param_string = param_string.strip()
    if not param_string:
        return
    
    # Check if it's a KEY=VALUE pair
    if '=' in param_string:
        # Find the first '=' that's not inside parentheses or quotes
        eq_pos = _find_first_equals(param_string)
        
        if eq_pos > 0:
            key = param_string[:eq_pos].strip()
            value = param_string[eq_pos+1:].strip()
            
            # Clean up key (should be alphanumeric + underscore)
            key = key.upper()
            
            params_dict[key] = value
        else:
            # No valid '=' found, treat as value-only parameter
            params_dict[param_string] = ""
    else:
        # No '=', this is a standalone parameter (e.g., DUMMY, *)
        params_dict[param_string] = ""


def _find_first_equals(param_string):
    """
    Find the position of the first '=' that's not inside parentheses or quotes.
    
    Args:
        param_string (str): The parameter string to search
        
    Returns:
        int: Position of the first valid '=', or -1 if not found
    """
    paren_depth = 0
    quote_char = None
    
    for i, char in enumerate(param_string):
        if char in ('"', "'"):
            if quote_char is None:
                quote_char = char
            elif char == quote_char:
                quote_char = None
        elif char == '(' and quote_char is None:
            paren_depth += 1
        elif char == ')' and quote_char is None:
            paren_depth -= 1
        elif char == '=' and paren_depth == 0 and quote_char is None:
            return i
    
    return -1


def fix_jcl_parameters_in_result(jcl_result):
    """
    Post-process a JCL parsing result to fix all parameter dictionaries.
    
    This function walks through the JCL structure (job, steps, dd_statements)
    and re-parses all parameter dictionaries using the generic parse_jcl_parameters function.
    
    Args:
        jcl_result (dict): The parsed JCL structure from legacylens
        
    Returns:
        dict: The JCL structure with corrected parameters
    """
    if not isinstance(jcl_result, dict):
        return jcl_result
    
    # Fix job parameters if present
    if 'job' in jcl_result and jcl_result['job']:
        job = jcl_result['job']
        if 'parameters' in job and isinstance(job['parameters'], dict):
            _fix_parameters_dict(job['parameters'])
    
    # Fix step parameters and DD statements
    if 'steps' in jcl_result and isinstance(jcl_result['steps'], list):
        for step in jcl_result['steps']:
            if not isinstance(step, dict):
                continue
            
            # Fix step parameters
            if 'parameters' in step and isinstance(step['parameters'], dict):
                _fix_parameters_dict(step['parameters'])
            
            # Fix DD statement parameters
            if 'dd_statements' in step and isinstance(step['dd_statements'], list):
                for dd in step['dd_statements']:
                    if isinstance(dd, dict) and 'parameters' in dd:
                        if isinstance(dd['parameters'], dict):
                            _fix_parameters_dict(dd['parameters'])
    
    return jcl_result


def _fix_parameters_dict(params):
    """
    Fix a single parameters dictionary by re-parsing from _value.
    
    Args:
        params (dict): The parameters dictionary to fix (modified in place)
    """
    if not isinstance(params, dict):
        return
    
    # Get the original _value string
    value_string = params.get('_value', '')
    
    if not value_string:
        # No _value to re-parse, keep as is
        return
    
    # Re-parse the parameters
    new_params = parse_jcl_parameters(value_string)
    
    # Clear old parameters (except _value)
    old_value = params.pop('_value', '')
    params.clear()
    
    # Add the re-parsed parameters
    params.update(new_params)
    
    # Always keep _value as the source of truth
    params['_value'] = old_value
