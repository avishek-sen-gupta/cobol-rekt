from __future__ import annotations

from typing import Callable, Any

import networkx as nx
from dotenv import load_dotenv
from networkx import Graph

from src.common.analysis_edge import AnalysisEdge
from src.common.data_node import DataNode
from src.common.flow_node import FlowNode

load_dotenv("env/.env", override=True)


def extract(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge],
            should_extract_code: Callable[[FlowNode], bool] = lambda n: True,
            should_extract_data: Callable[[DataNode], bool] = lambda d: True,
            should_extract_edge: Callable[[AnalysisEdge], bool] = lambda e: True) -> Graph:
    all_nodes: dict[str, FlowNode | DataNode] = {}
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


def extract_ast(unified: dict[str, Any]) -> Graph:
    return _extract_ast(*json_elements(unified))


def extract_cfg(unified: dict[str, Any]) -> Graph:
    # code_nodes = [FlowNode.from_dict(v) for v in unified["codeVertices"]]
    # data_nodes = [DataNode.from_dict(v) for v in unified["dataVertices"]]
    # edges = [AnalysisEdge.from_dict(v) for v in unified["edges"]]
    # return _extract_cfg(code_nodes, data_nodes, edges)
    return _extract_cfg(*json_elements(unified))


def extract_ds(unified: dict[str, Any]) -> Graph:
    # code_nodes = [FlowNode.from_dict(v) for v in unified["codeVertices"]]
    # data_nodes = [DataNode.from_dict(v) for v in unified["dataVertices"]]
    # edges = [AnalysisEdge.from_dict(v) for v in unified["edges"]]
    # return _extract_ds(code_nodes, data_nodes, edges)
    return _extract_ds(*json_elements(unified))


def _extract_ast(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]) -> Graph:
    return extract(code_nodes, data_nodes, edges,
                   lambda n: True,
                   lambda d: False,
                   lambda e: e.edge_type == "CONTAINS_CODE")


def _extract_cfg(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]) -> Graph:
    return extract(code_nodes, data_nodes, edges,
                   lambda n: True,
                   lambda d: False,
                   lambda e: e.edge_type == "STARTS_WITH" or e.edge_type == "FOLLOWED_BY")


def _extract_ds(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]) -> Graph:
    return extract(code_nodes, data_nodes, edges,
                   lambda n: False,
                   lambda d: True,
                   lambda e: e.edge_type == "CONTAINS_DATA" or
                             e.edge_type == "FLOWS_INTO" or
                             e.edge_type == "REDEFINES")


def json_elements(unified) -> tuple[list[FlowNode], list[DataNode], list[AnalysisEdge]]:
    code_nodes = [FlowNode.from_dict(v) for v in unified["codeVertices"]]
    data_nodes = [DataNode.from_dict(v) for v in unified["dataVertices"]]
    edges = [AnalysisEdge.from_dict(v) for v in unified["edges"]]
    return code_nodes, data_nodes, edges
