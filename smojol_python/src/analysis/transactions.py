import argparse

from dotenv import load_dotenv

from src.llm.common.console_colors import ConsoleColors
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)
c = ConsoleColors()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="build_capabilities_graph")
    parser.add_argument(ParameterConstants.CAPABILITIES_PATH)
    args = parser.parse_args()
    capabilities_path = getattr(args, ParameterConstants.CAPABILITIES_PATH)


