import argparse
import json
import pygraphviz

import networkx as nx
from dotenv import load_dotenv

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
        pos = nx.random_layout(ast_x)
        nx.nx_agraph.write_dot(ast_x, "/Users/asgupta/code/smojol/out/nxstuff.dot")
        # nx.draw(ast_x, pos, with_labels=False)
        # plt.show()
    # print(paragraphs)
