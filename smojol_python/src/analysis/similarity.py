import argparse
import json

import networkx as nx

from src.common.flow_node import FlowNode
from src.ingestion.to_networkx import flow_node_to_networkx
from src.ingestion.unified_model_to_networkx import extract_ast
from src.llm.common.parameter_constants import ParameterConstants


def similarity(left: FlowNode, right: FlowNode) -> float:
    subgraph1, _ = flow_node_to_networkx(left)
    subgraph2, _ = flow_node_to_networkx(right)
    distance = nx.graph_edit_distance(subgraph1, subgraph2, roots=(left, right))
    return distance


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="similarity")
    parser.add_argument(ParameterConstants.FLOW_AST_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.FLOW_AST_PATH)
    with open(input_path, 'r') as file:
        ast = json.load(file)
        graph, ast_root, data_root = extract_ast(ast)

        paragraphs = ast_root.nodes(lambda n: n.type == "PARAGRAPH")
        left = paragraphs[0]
        right = paragraphs[10]

        print(left.name + " <-> " + right.name)
        print(similarity(left, right))

        # pos = nx.random_layout(graph)
        # nx.draw(graph, pos, with_labels=False)
        # plt.show()
        # print(paragraphs)
