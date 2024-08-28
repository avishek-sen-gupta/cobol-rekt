from functools import reduce
from typing import Any, Callable

ASTNode = dict[str, Any]
SearchPath = Callable[[ASTNode], ASTNode]
Criterion = Callable[[ASTNode], bool]


def null_node() -> ASTNode:
    return {"children": [], "nodeType": "NULL"}


def is_null_node(node: ASTNode) -> bool:
    return node["nodeType"] == "NULL"


def at_least_one(final: ASTNode, c: ASTNode) -> ASTNode:
    if not is_null_node(final):
        return final
    if not is_null_node(c):
        return c
    return null_node()


def direct(criterion: Criterion) -> SearchPath:
    def apply(node) -> dict[str, Any]:
        found_children = list(filter(lambda n: criterion(n), node["children"]))
        return reduce(at_least_one, found_children, null_node())

    return apply


def recursive(criterion: Criterion) -> SearchPath:
    def apply(node: ASTNode) -> ASTNode:
        print(f"Checking node of type: {node['nodeType']}")
        if criterion(node):
            return node
        return reduce(at_least_one, list(map(apply, node["children"])), null_node())

    return apply


def initial_candidates(criterion: Criterion, tree: ASTNode) -> list[ASTNode]:
    if criterion(tree):
        return [tree]
    if len(tree["children"]) == 0:
        return []
    return reduce(lambda prev, curr: prev + curr, map(lambda n: initial_candidates(criterion, n), tree["children"]), [])


def find_where(tree: ASTNode, criterion: Criterion, conditions: list[SearchPath]) -> list[ASTNode]:
    candidate_nodes = initial_candidates(criterion, tree)
    return list(filter(lambda node: not is_null_node(reduce(lambda prev, path: path(prev), conditions, node)),
                       candidate_nodes))


def path(search_root: ASTNode, conditions: list[SearchPath]) -> ASTNode:
    return reduce(lambda prev, search_path: search_path(prev), conditions, search_root)


def node_type(ntype: str) -> Callable[[ASTNode], bool]:
    def apply(node: dict[str, Any]) -> bool:
        return node["nodeType"] == ntype

    return apply
