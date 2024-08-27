import argparse
import json
from jsonpath_ng.ext import parse

from dotenv import load_dotenv

from src.llm.common.console_colors import ConsoleColors
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)
c = ConsoleColors()


def direct(node_type):


def recursive(node_type):
    pass


def recurse_operand(node_type, tree):
    matching = []
    if tree["nodeType"] == node_type:
        return [tree]
    for node in tree["children"]:
        matching += recurse_operand(node_type, node)
    return matching

def flter(tree, node_type, condition):
    matching = recurse_operand(node_type, tree)
    filter(condition, )


if __name__ == "__main__":
    # parser = argparse.ArgumentParser(prog="variable_values")
    # parser.add_argument(ParameterConstants.RAW_AST_PATH)
    # args = parser.parse_args()

    input_path = "/Users/asgupta/code/smojol/out/report/test-exp.cbl.report/ast/cobol-test-exp.cbl.json"
    # input_path = getattr(args, ParameterConstants.RAW_AST_PATH)

    with open(input_path, 'r') as file:
        ast = json.load(file)
        flter(ast, "MoveToStatementContext", [direct("GeneralIdentifierContext"), recursive("VariableUsageNameContext")])
        # jp = parse("$..*[?(@.nodeType == 'VariableUsageNameContext' & @.children[0].nodeType == 'CobolWordContext')]")
        jp = parse("$..*[?(@.nodeType == 'VariableUsageNameContext' & @.`parent`.nodeType == 'MoveToStatementContext')]")
        jp = parse("$..*[?(@.nodeType == 'MoveToStatementContext' & @.children[?(@.nodeType == 'GeneralIdentifierContext')])]")
        sections = [match.value for match in jp.find(ast)]
        pass
