import argparse
import json

from dotenv import load_dotenv

from src.llm.common.console_colors import ConsoleColors
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)
c = ConsoleColors()

if __name__ == "__main__":
    # parser = argparse.ArgumentParser(prog="variable_values")
    # parser.add_argument(ParameterConstants.RAW_AST_PATH)
    # args = parser.parse_args()

    input_path = "/Users/asgupta/code/smojol/out/report/test-exp.cbl.report/ast/cobol-test-exp.cbl.json"
    # input_path = getattr(args, ParameterConstants.RAW_AST_PATH)

    with open(input_path, 'r') as file:
        ast = json.load(file)
        pass
