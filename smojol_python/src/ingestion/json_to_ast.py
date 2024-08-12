from __future__ import annotations

import json as json

import networkx as nx
from networkx import Graph, DiGraph

from src.common.flow_node import FlowNode
from src.ingestion.networkx_visitor import NetworkXVisitor


def recursively_build(json_node: dict) -> FlowNode:
    node = FlowNode(json_node["id"], json_node["label"], json_node["name"], json_node["originalText"],
                    json_node["type"])
    children = [recursively_build(json_child) for json_child in json_node["children"]]
    node.add_all(children)
    return node


if __name__ == "__main__":
    with open("/Users/asgupta/code/smojol/out/report/test-exp.cbl.report/flow_ast/flow-ast-test-exp.cbl.json",
              'r') as file:
        ast = json.load(file)
        root = recursively_build(ast)
        graph: Graph = DiGraph()
        subgraph1: Graph = DiGraph()
        subgraph2: Graph = DiGraph()
        root.accept(NetworkXVisitor(None, graph))
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
