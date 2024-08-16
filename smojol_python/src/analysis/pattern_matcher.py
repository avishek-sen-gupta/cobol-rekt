import argparse
import json

import networkx as nx
from matplotlib import pyplot as plt

from src.ingestion.unified_model_to_networkx import extract_ast, extract_cfg, extract_ds
from src.llm.common.parameter_constants import ParameterConstants

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="unified_model_to_networkx")
    parser.add_argument(ParameterConstants.UNIFIED_MODEL_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.UNIFIED_MODEL_PATH)
    with open(input_path, 'r') as file:
        unified = json.load(file)
        ast_x = extract_ast(unified)
        cfg_x = extract_cfg(unified)
        ds_x = extract_ds(unified)
        pos = nx.kamada_kawai_layout(ast_x)
        nx.draw(ast_x, pos, with_labels=False)
        plt.show()
    # print(paragraphs)
