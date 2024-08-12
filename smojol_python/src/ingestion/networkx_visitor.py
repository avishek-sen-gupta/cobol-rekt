from __future__ import annotations

from networkx import Graph

from src.common.flow_node import FlowNode, FlowNodeVisitor


class NetworkXVisitor:
    def __init__(self, parent: FlowNode | None, g: Graph):
        self.parent = parent
        self.g = g

    def visit(self, node: FlowNode) -> FlowNodeVisitor:
        self.g.add_node(node)
        if self.parent is not None:
            self.g.add_edge(self.parent, node)
        # print(node.name)
        return NetworkXVisitor(node, self.g)
