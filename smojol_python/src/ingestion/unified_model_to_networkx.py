from __future__ import annotations

import json

import networkx as nx
from matplotlib import pyplot as plt

from src.common.analysis_edge import AnalysisEdge
from src.common.data_node import DataNode
from src.common.flow_node import FlowNode

if __name__ == "__main__":
    with open("/Users/asgupta/code/smojol/out/test.json",
              'r') as file:
        unified = json.load(file)
        code_nodes = [FlowNode.from_dict(v) for v in unified["codeVertices"]]
        data_nodes = [DataNode.from_dict(v) for v in unified["dataVertices"]]
        all_nodes = {}

        edges = [AnalysisEdge.from_dict(v) for v in unified["edges"]]

        graph = nx.MultiDiGraph()
        for n in code_nodes:
            all_nodes[n.id] = n
            graph.add_node(n)

        for n in data_nodes:
            all_nodes[n.id] = n
            graph.add_node(n)

        for e in edges:
            if e.from_node_id not in all_nodes:
                raise ModuleNotFoundError(f"From-Node with ID {e.from_node_id} was not found!")
            if e.to_node_id not in all_nodes:
                raise ModuleNotFoundError(f"To-Node with ID {e.to_node_id} was not found!")
            graph.add_edge(all_nodes[e.from_node_id], all_nodes[e.to_node_id], edge_type=e.edge_type)

        pos = nx.kamada_kawai_layout(graph)
        nx.draw(graph, pos, with_labels=False)
        plt.show()
    # print(paragraphs)
