from __future__ import annotations


class AnalysisEdge:
    def __init__(self, edge_id: str, from_node_id: str, to_node_id: str, edge_type: str):
        self.id = edge_id
        self.from_node_id = from_node_id
        self.to_node_id = to_node_id
        self.edge_type = edge_type

    @classmethod
    def from_dict(cls, v):
        return cls(v["id"], v["fromNodeID"], v["toNodeID"], v["edgeType"])
