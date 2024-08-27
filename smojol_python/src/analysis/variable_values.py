import argparse
import json
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


def direct(node_type):
    def apply(node):
        if isinstance(node, NullNode):
            return NullNode()
        children = node["children"]
        if len(children) == 0:
            return NullNode()
        found_children = list(filter(lambda n: n["nodeType"] == node_type, children))
        if len(found_children) == 0:
            return NullNode()
        return found_children[0]

    return apply


def recursive(node_type):
    def apply(node):
        print(f"Checking node of type: {node['nodeType']}")
        if isinstance(node, NullNode):
            return NullNode()
        if node["nodeType"] == node_type:
            return node
        if len(node["children"]) == 0:
            return NullNode()
        for child in node["children"]:
            found_descendant = apply(child)
            if isinstance(found_descendant, NullNode):
                continue
            return found_descendant
        return NullNode()

    return apply


def recurse_operand(node_type, tree):
    matching = []
    if tree["nodeType"] == node_type:
        return [tree]
    for node in tree["children"]:
        matching += recurse_operand(node_type, node)
    return matching


def flter(tree, node_type, conditions):
    matching_nodes = recurse_operand(node_type, tree)
    final_nodes = []
    for node in matching_nodes:
        current_level = node
        for condition in conditions:
            current_level = condition(current_level)
        if isinstance(current_level, NullNode):
            continue
        final_nodes.append(node)
    return final_nodes


if __name__ == "__main__":
    # parser = argparse.ArgumentParser(prog="variable_values")
    # parser.add_argument(ParameterConstants.RAW_AST_PATH)
    # args = parser.parse_args()

    input_path = "/Users/asgupta/code/smojol/out/report/test-exp.cbl.report/ast/cobol-test-exp.cbl.json"
    # input_path = getattr(args, ParameterConstants.RAW_AST_PATH)

    with open(input_path, 'r') as file:
        ast = json.load(file)
        final_nodes = flter(ast, "AddStatementContext",
                            [recursive("AddFromContext"), recursive("GeneralIdentifierContext")])
        # jp = parse("$..*[?(@.nodeType == 'VariableUsageNameContext' & @.children[0].nodeType == 'CobolWordContext')]")
        # jp = parse(
        #     "$..*[?(@.nodeType == 'VariableUsageNameContext' & @.`parent`.nodeType == 'MoveToStatementContext')]")
        # jp = parse(
        #     "$..*[?(@.nodeType == 'MoveToStatementContext' & @.children[?(@.nodeType == 'GeneralIdentifierContext')])]")
        # sections = [match.value for match in jp.find(ast)]
        pass
