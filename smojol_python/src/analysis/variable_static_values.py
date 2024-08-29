import argparse
import json

from dotenv import load_dotenv

from src.common.json_navigator import find_where, node_type, recursive, path, ASTNode
from src.llm.common.console_colors import ConsoleColors
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)
c = ConsoleColors()


def variable_static_values(root: ASTNode) -> dict[str, set]:
    final_nodes = find_where(root, node_type("MoveStatementContext"),
                             [recursive(node_type("MoveToSendingAreaContext")),
                              recursive(node_type("LiteralContext"))])
    static_assignment_pairs = [
        (path(n, [recursive(node_type("MoveToStatementContext")), recursive(node_type("VariableUsageNameContext"))])[
             "text"],
         path(n, [recursive(node_type("MoveToSendingAreaContext")), recursive(node_type("LiteralContext"))])["text"])
        for n
        in
        final_nodes]
    value = path(final_nodes[0],
                 [recursive(node_type("MoveToSendingAreaContext")), recursive(node_type("LiteralContext"))])
    static_assignments: dict[str, set] = {}
    for variable, value in static_assignment_pairs:
        if variable not in static_assignments:
            static_assignments[variable] = set()
        static_assignments[variable].add(value)
    return static_assignments


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="variable_values")
    parser.add_argument(ParameterConstants.RAW_AST_PATH)
    parser.add_argument("--" + ParameterConstants.VARIABLE_STATIC_VALUES_OUTPUT_PATH, required=False)
    args = parser.parse_args()

    input_path = getattr(args, ParameterConstants.RAW_AST_PATH)
    output_path = getattr(args, ParameterConstants.VARIABLE_STATIC_VALUES_OUTPUT_PATH)

    with open(input_path, 'r') as file:
        ast = json.load(file)
        static_assignments = variable_static_values(ast)
        print(static_assignments)
        if not output_path:
            exit(0)
        serialisable_assignments: list[tuple[str, list[str]]] = [(key, list(values)) for key, values in
                                                                 static_assignments.items()]
        with open(output_path, "w") as outfile:
            json.dump(serialisable_assignments, outfile, indent=4, sort_keys=True)
