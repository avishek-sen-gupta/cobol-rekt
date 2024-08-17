from __future__ import annotations

from typing import Callable, Any, Optional, Union

import networkx as nx
from networkx import Graph

from src.common.analysis_edge import AnalysisEdge
from src.common.data_node import DataNode
from src.common.flow_node import FlowNode


def extract(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge],
            should_extract_code: Callable[[FlowNode], bool] = lambda n: True,
            should_extract_data: Callable[[DataNode], bool] = lambda d: True,
            should_extract_edge: Callable[[AnalysisEdge], bool] = lambda e: True) -> (
        tuple)[Graph, FlowNode, DataNode]:
    all_code_nodes: dict[str, FlowNode] = {}
    all_data_nodes: dict[str, DataNode] = {}
    graph = nx.MultiDiGraph()
    for n in code_nodes:
        all_code_nodes[n.id] = n
        if not should_extract_code(n):
            continue
        graph.add_node(n, id=n.id, obj=n)

    for e in edges:
        if not should_extract_edge(e):
            continue
        if e.edge_type == "CONTAINS_CODE":
            all_code_nodes[e.from_node_id].add_child(all_code_nodes[e.to_node_id])
        elif e.edge_type == "STARTS_WITH":
            all_code_nodes[e.from_node_id].add_internal_root(all_code_nodes[e.to_node_id])
        elif e.edge_type == "FOLLOWED_BY":
            all_code_nodes[e.from_node_id].add_outgoing(all_code_nodes[e.to_node_id])

    for d in data_nodes:
        all_data_nodes[d.id] = d
        if not should_extract_data(d):
            continue
        graph.add_node(d)

    # Nodes are added automatically, but we need to add them explicitly above to add extra attributes
    for e in edges:
        if not should_extract_edge(e):
            continue
        from_node = node(e.from_node_id, all_code_nodes, all_data_nodes)
        to_node = node(e.to_node_id, all_code_nodes, all_data_nodes)

        if not from_node:
            raise ModuleNotFoundError(f"From-Node with ID {e.from_node_id} was not found!")
        if not to_node:
            raise ModuleNotFoundError(f"To-Node with ID {e.to_node_id} was not found!")

        graph.add_edge(from_node, to_node, edge_type=e.edge_type)

    ast_root = [ast_root for ast_root in code_nodes if ast_root.type == "PROCEDURE_DIVISION_BODY"][0]
    ds_root = [ds_root for ds_root in data_nodes if ds_root.data_type == "ROOT"][0]
    return graph, ast_root, ds_root


def node(node_id, all_code_nodes, all_data_nodes) -> Union[FlowNode | DataNode | None]:
    if node_id in all_code_nodes:
        return all_code_nodes[node_id]
    elif node_id in all_data_nodes:
        return all_data_nodes[node_id]
    return None


def extract_ast(unified: dict[str, Any]) -> tuple[Graph, FlowNode, DataNode]:
    return _extract_ast(*json_elements(unified))


def extract_cfg(unified: dict[str, Any]) -> tuple[Graph, FlowNode, DataNode]:
    return _extract_cfg(*json_elements(unified))


def extract_ds(unified: dict[str, Any]) -> tuple[Graph, FlowNode, DataNode]:
    return _extract_ds(*json_elements(unified))


def _extract_ast(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]) -> (
        tuple)[Graph, FlowNode, DataNode]:
    return extract(code_nodes, data_nodes, edges,
                   lambda n: True,
                   lambda d: False,
                   lambda e: e.edge_type == "CONTAINS_CODE")


def _extract_cfg(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]) -> (
        tuple)[Graph, FlowNode, DataNode]:
    return extract(code_nodes, data_nodes, edges,
                   lambda n: True,
                   lambda d: False,
                   lambda e: e.edge_type == "STARTS_WITH" or e.edge_type == "FOLLOWED_BY")


def _extract_ds(code_nodes: list[FlowNode], data_nodes: list[DataNode], edges: list[AnalysisEdge]) -> (
        tuple)[Graph, FlowNode, DataNode]:
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
