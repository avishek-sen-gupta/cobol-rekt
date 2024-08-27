import argparse
import json
from functools import reduce

from jsonpath_ng.ext import parse

from dotenv import load_dotenv

from src.llm.common.console_colors import ConsoleColors
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)
c = ConsoleColors()


class NullNode:
    def __init__(self):
        self.dict = {"children": [], "nodeType": "NULL"}


def __getitem__(self, item):
    return self.dict[item]


def lol(final, c):
    if not isinstance(final, NullNode):
        return final
    if not isinstance(c, NullNode):
        return c
    return NullNode()


def direct(criterion):
    def apply(node):
        found_children = list(filter(lambda n: criterion(n), node["children"]))
        return reduce(lol, found_children, NullNode())

    return apply


def recursive(criterion):
    def apply(node):
        print(f"Checking node of type: {node['nodeType']}")
        if criterion(node):
            return node
        return reduce(lol, list(map(apply, node["children"])), NullNode())

    return apply


def recurse_operand(criterion, tree):
    matching = []
    if criterion(tree):
        return [tree]
    # map(lambda n: recurse_operand(criterion, node), tree["children"])
    for node in tree["children"]:
        matching += recurse_operand(criterion, node)
    return matching


def flter(tree, node_type, conditions):
    matching_nodes = recurse_operand(node_type, tree)
    return list(filter(lambda node: not isinstance(reduce(lambda prev, search: search(prev), conditions, node), NullNode), matching_nodes))


def node_type(ntype):
    def apply(node):
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
        final_nodes = flter(ast, node_type("AddStatementContext"),
                            [recursive(node_type("AddFromContext")),
                                       recursive(node_type("GeneralIdentifierContext"))])
        # jp = parse("$..*[?(@.nodeType == 'VariableUsageNameContext' & @.children[0].nodeType == 'CobolWordContext')]")
        # jp = parse(
        #     "$..*[?(@.nodeType == 'VariableUsageNameContext' & @.`parent`.nodeType == 'MoveToStatementContext')]")
        # jp = parse(
        #     "$..*[?(@.nodeType == 'MoveToStatementContext' & @.children[?(@.nodeType == 'GeneralIdentifierContext')])]")
        # sections = [match.value for match in jp.find(ast)]
        pass
