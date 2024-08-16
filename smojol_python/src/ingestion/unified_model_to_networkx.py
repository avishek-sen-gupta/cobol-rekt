from __future__ import annotations

import argparse
import json
from typing import Callable

import networkx as nx
from dotenv import load_dotenv
from matplotlib import pyplot as plt
from networkx import Graph

from src.common.analysis_edge import AnalysisEdge
from src.common.data_node import DataNode
from src.common.flow_node import FlowNode
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)


def extract(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge],
            should_extract_code: Callable[[FlowNode], bool] = lambda n: True,
            should_extract_data: Callable[[DataNode], bool] = lambda d: True,
            should_extract_edge: Callable[[AnalysisEdge], bool] = lambda e: True) -> Graph:
    graph = nx.MultiDiGraph()
    for n in code_nodes:
        if not should_extract_code(n):
            continue
        all_nodes[n.id] = n
        graph.add_node(n)

    for n in data_nodes:
        if not should_extract_data(n):
            continue
        all_nodes[n.id] = n
        graph.add_node(n)

    for e in edges:
        if not should_extract_edge(e):
            continue
        if e.from_node_id not in all_nodes:
            raise ModuleNotFoundError(f"From-Node with ID {e.from_node_id} was not found!")
        if e.to_node_id not in all_nodes:
            raise ModuleNotFoundError(f"To-Node with ID {e.to_node_id} was not found!")
        graph.add_edge(all_nodes[e.from_node_id], all_nodes[e.to_node_id], edge_type=e.edge_type)
    return graph


def extract_ast(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]):
    return extract(code_nodes, data_nodes, edges,
                   lambda n: True,
                   lambda d: False,
                   lambda e: e.edge_type == "CONTAINS_CODE")


def extract_cfg(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]):
    return extract(code_nodes, data_nodes, edges,
                   lambda n: True,
                   lambda d: False,
                   lambda e: e.edge_type == "STARTS_WITH" or e.edge_type == "FOLLOWED_BY")


def extract_ds(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]):
    return extract(code_nodes, data_nodes, edges,
                   lambda n: False,
                   lambda d: True,
                   lambda e: e.edge_type == "CONTAINS_DATA" or
                             e.edge_type == "FLOWS_INTO" or
                             e.edge_type == "REDEFINES")


def extract_ds(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]):
    return extract(code_nodes, data_nodes, edges,
                   lambda n: False,
                   lambda d: True,
                   lambda e: e.edge_type == "CONTAINS_DATA" or e.edge_type == "REDEFINES")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="unified_model_to_networkx")
    parser.add_argument(ParameterConstants.UNIFIED_MODEL_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.UNIFIED_MODEL_PATH)
    with open(input_path, 'r') as file:
        unified = json.load(file)
        code_nodes = [FlowNode.from_dict(v) for v in unified["codeVertices"]]
        data_nodes = [DataNode.from_dict(v) for v in unified["dataVertices"]]
        all_nodes = {}

        edges = [AnalysisEdge.from_dict(v) for v in unified["edges"]]

        g = extract_ast(code_nodes, data_nodes, edges)
        g = extract_cfg(code_nodes, data_nodes, edges)
        g = extract_ds(code_nodes, data_nodes, edges)
        pos = nx.spiral_layout(g)
        nx.draw(g, pos, with_labels=False)
        plt.show()
    # print(paragraphs)
