from __future__ import annotations

from typing import Any

from networkx import Graph, DiGraph

from src.common.flow_node import FlowNode
from src.ingestion.networkx_visitor import NetworkXVisitor


def recursively_build(json_node: dict) -> FlowNode:
    node = FlowNode(json_node["id"], json_node["label"], json_node["name"], json_node["originalText"],
                    json_node["type"], json_node["nodeType"])
    children = [recursively_build(json_child) for json_child in json_node["children"]]
    node.add_all(children)
    return node


def to_networkx(ast_json: dict[str, Any]) -> tuple[Graph, FlowNode]:
    root = recursively_build(ast_json)
    graph: Graph = DiGraph()
    root.accept(NetworkXVisitor(None, graph))
    return graph, root
