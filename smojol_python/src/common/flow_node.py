from __future__ import annotations

import uuid
from functools import reduce
from typing import Callable, Protocol


class FlowNode:
    def __init__(self, node_id: str, label: str, name: str, original_text: str, node_type: str, node_category: str):
        self.internal_roots: list[FlowNode] = []
        self.node_category = node_category
        self.children: list[FlowNode] = []
        self.outgoing_nodes: list[FlowNode] = []
        self.incoming_nodes: list[FlowNode] = []
        self.type = node_type
        self.original_text = original_text
        self.name = name
        self.label = label
        self.id = node_id

    def add_all(self, children: list[FlowNode]) -> None:
        self.children += children

    def add_child(self, child: FlowNode) -> None:
        self.children.append(child)

    def add_outgoing(self, outgoing: FlowNode) -> None:
        self.outgoing_nodes.append(outgoing)
        outgoing.add_incoming(self)

    def add_incoming(self, incoming: FlowNode) -> None:
        self.incoming_nodes.append(incoming)

    def add_internal_root(self, internal_root: FlowNode) -> None:
        self.internal_roots.append(internal_root)

    def accept(self, visitor: FlowNodeVisitor) -> None:
        scoped_visitor = visitor.visit(self)
        for child in self.children:
            child.accept(scoped_visitor)

    def nodes_(self, criterion: Callable[[FlowNode], bool]) -> list[FlowNode]:
        self_match = [self] if criterion(self) else []
        initial: list[FlowNode] = []
        return self_match + reduce(lambda acc, n: acc + n.nodes_(criterion), self.children, initial)

    def nodes(self, criterion: Callable[[FlowNode], bool]) -> list[FlowNode]:
        return list(filter(lambda n: n is not None, self.nodes_(criterion)))

    def __str__(self):
        return self.original_text

    def __eq__(self, other):
        if not isinstance(other, FlowNode):
            return False
        return self.id == other.id

    def __hash__(self):
        return uuid.UUID(self.id).int

    @classmethod
    def from_dict(cls, v):
        return cls(v["id"], v["label"], v["name"], v["originalText"], v["type"], v["nodeType"])


class FlowNodeVisitor(Protocol):
    def visit(self, node: FlowNode) -> FlowNodeVisitor:
        ...
