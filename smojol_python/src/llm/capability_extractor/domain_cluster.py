from __future__ import annotations

from typing import Optional


class DomainCluster:
    def __init__(self, index: int, words: list[str], children: list[DomainCluster] = []):
        self.parent: Optional[DomainCluster] = None
        self.umbrella_subdomain: str = ""
        self.index: int = index
        self.children: list[DomainCluster] = children
        self.composite: bool = not index < len(words)
        self.domain: str = words[index] if index < len(words) else f"CLUSTER-{index}"
        self.content: str = self.domain
        for child in self.children:
            child.parent = self
