from __future__ import annotations

from functools import reduce
from typing import Callable, Protocol


class FlowNode:
    def __init__(self, node_id: str, label: str, name: str, original_text: str, node_type: str):
        self.children: list[FlowNode] = []
        self.type = node_type
        self.original_text = original_text
        self.name = name
        self.label = label
        self.id = node_id

    def add_all(self, children: list[FlowNode]) -> None:
        self.children += children

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

    @classmethod
    def from_dict(cls, v):
        return cls(v["id"], v["label"], v["name"], v["originalText"], v["type"])


class FlowNodeVisitor(Protocol):
    def visit(self, node: FlowNode) -> FlowNodeVisitor:
        ...
