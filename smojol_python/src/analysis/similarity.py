import argparse
import json

import networkx as nx
from networkx import DiGraph, Graph

from src.ingestion.flow_ast_json_to_networkx import to_networkx
from src.ingestion.networkx_visitor import NetworkXVisitor
from src.llm.common.parameter_constants import ParameterConstants

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="flow_ast_to_networkx")
    parser.add_argument(ParameterConstants.FLOW_AST_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.FLOW_AST_PATH)
    with open(input_path, 'r') as file:
        ast = json.load(file)
        graph, root = to_networkx(ast)

        subgraph1: Graph = DiGraph()
        subgraph2: Graph = DiGraph()
        # print(list(map(lambda n: n.name, list(graph.nodes))))
        paragraphs = root.nodes(lambda n: n.type == "PARAGRAPH")
        left = paragraphs[0]
        right = paragraphs[1]

        left.accept(NetworkXVisitor(None, subgraph1))
        right.accept(NetworkXVisitor(None, subgraph2))

        distance = nx.graph_edit_distance(subgraph1, subgraph2, roots=(left, right))
        print(distance)
        # print(list(map(lambda n: n.name, list(subgraph1.nodes))))
        # print(list(subgraph1.nodes))

        # pos = nx.kamada_kawai_layout(graph)
        # nx.draw(graph, pos, with_labels=False)
        # plt.show()
        # print(paragraphs)
