import argparse
import json
from functools import reduce
from typing import Callable, Any

from jsonpath_ng.ext import parse

from dotenv import load_dotenv

from src.llm.common.console_colors import ConsoleColors
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)
c = ConsoleColors()

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


def recurse_operand(criterion: Criterion, tree: ASTNode) -> list[ASTNode]:
    if criterion(tree):
        return [tree]
    if len(tree["children"]) == 0:
        return []
    return reduce(lambda prev, curr: prev + curr, map(lambda n: recurse_operand(criterion, n), tree["children"]), [])


def find_where(tree: ASTNode, criterion: Criterion, conditions: list[SearchPath]) -> list[ASTNode]:
    matching_nodes = recurse_operand(criterion, tree)
    return list(filter(lambda node: not is_null_node(reduce(lambda prev, path: path(prev), conditions, node)),
                       matching_nodes))


def node_type(ntype: str) -> Callable[[ASTNode], bool]:
    def apply(node: dict[str, Any]) -> bool:
        return node["nodeType"] == ntype

    return apply


if __name__ == "__main__":
    # parser = argparse.ArgumentParser(prog="variable_values")
    # parser.add_argument(ParameterConstants.RAW_AST_PATH)
    # args = parser.parse_args()

    input_path = "/Users/asgupta/code/smojol/out/report/test-exp.cbl.report/ast/cobol-test-exp.cbl.json"
    # input_path = getattr(args, ParameterConstants.RAW_AST_PATH)

    with open(input_path, 'r') as file:
        ast = json.load(file)
        final_nodes = find_where(ast, node_type("AddStatementContext"),
                                 [recursive(node_type("AddFromContext")),
                                  recursive(node_type("GeneralIdentifierContext"))])
        # jp = parse("$..*[?(@.nodeType == 'VariableUsageNameContext' & @.children[0].nodeType == 'CobolWordContext')]")
        # jp = parse(
        #     "$..*[?(@.nodeType == 'VariableUsageNameContext' & @.`parent`.nodeType == 'MoveToStatementContext')]")
        # jp = parse(
        #     "$..*[?(@.nodeType == 'MoveToStatementContext' & @.children[?(@.nodeType == 'GeneralIdentifierContext')])]")
        # sections = [match.value for match in jp.find(ast)]
        pass
