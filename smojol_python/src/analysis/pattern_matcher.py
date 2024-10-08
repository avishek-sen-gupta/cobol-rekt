import argparse
import json

from dotenv import load_dotenv

from src.common.pattern.pattern_builder import sentence_sequence_pattern, match_pattern, call_pattern
from src.ingestion.unified_model_to_networkx import extract_ast, extract_cfg, extract_ds
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="pattern_matcher")
    parser.add_argument(ParameterConstants.UNIFIED_MODEL_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.UNIFIED_MODEL_PATH)
    with open(input_path, 'r') as file:
        unified = json.load(file)
        ast_x, _, _ = extract_ast(unified)
        cfg_x, _, _ = extract_cfg(unified)
        ds_x, _, _ = extract_ds(unified)
        x, _, _ = call_pattern(5)
        # x, _ = sentence_sequence_pattern("MOVE", 18)
        match_pattern(x, cfg_x)
