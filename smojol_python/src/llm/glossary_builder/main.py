import argparse
import json
import os
from typing import Any

from langchain_openai import AzureChatOpenAI

from src.llm.common.env_vars import openai_config, neo4j_config
from src.llm.common.parameter_constants import ParameterConstants


def explain_through_llm(struct: dict[str, Any], path: list[str], llm: AzureChatOpenAI) -> None:
    print(f"Path is {path} for {struct['name']}")
    var_name = struct["name"]
    system_message = f"""
                                You are an expert of Mainframe and IDMS codebases.
                                The user will provide you with the name of a data structure as well as the hierarchy of that variable
                                in a list. This data structure exists in the automotive after-sales domain of a German company.
                                The code is written in a mix of Cobol and the IDMS dialect, so there will be mainframe terms mixed in throughout.
                                You need to explain the expansion of the name of the data structure.
                                Expand the name of the data structure with the name: '{var_name}'. Suggest alternative expansions as well. Consider the context as well when interpreting the variable name. Ignore variable names
                                like 'FILLER' and 'ROOT'. They are placeholders, so don't expand on them, or explain them. Also, in your responses, be as concise (but not unhelpful) and professional as possible.
                                The hierarchy is '{path}'
                            """
    while True:
        try:
            response = llm.invoke(system_message)
            print(response.content)
            break
        except Exception as e:
            print(e)
            continue


def collect(struct: dict[str, Any], path: list[str], llm: AzureChatOpenAI, all_paths: list[list[str]]) -> None:
    if "ROOT" not in struct["name"]:
        all_paths += [path + [struct["name"]]]
        # explain_through_llm(struct, path, llm)
    for child in struct["children"]:
        collect(child, path + [struct["name"]], llm, all_paths)


def explain_once(all_paths: list[list[str]], llm: AzureChatOpenAI) -> list[str]:
    print(f"Explaining {len(all_paths)} data structures")
    all_explanations: list[str] = []
    for x in range(0, len(all_paths), 20):
        all_explanations += explain_batch(all_paths[x:(x + 19)], llm, x, len(all_paths)) + "\n"
    return all_explanations


def explain_batch(all_paths: list[list[str]], llm: AzureChatOpenAI, index: int, max: int) -> str:
    print(f"Explaining {index} to {index + 19} of {max}...")
    system_message = f"""
                                You are an expert of Mainframe and IDMS codebases.
                                The user will provide you with the list of hierarchies of data structures.
                                Each entry in this list is the hierarchy of a particular data structure; the actual data structure is the
                                last entry in the child list. These data structures exist in the automotive after-sales domain of a German company.
                                The code is written in a mix of Cobol and the IDMS dialect, so there will be mainframe terms mixed in throughout.
                                You need to explain the expansion of all the parts of each of these data structure. Include a couple of sentences on
                                the importance of this data structure in the domain.
                                Do not re-explain a data structure if you have already expanded on it.
                                Definitely suggest alternative expansions as well. Consider the context as well when interpreting the variable name. Ignore variable names
                                like 'FILLER' and 'ROOT'. They are placeholders, so don't expand on them, or explain them. Also, in your responses, be as concise (but not unhelpful) and professional as possible.
                                The hierarchies are '{all_paths}'
                            """
    while True:
        try:
            response = llm.invoke(system_message)
            print(response.content)
            return response.content
        except Exception as e:
            print(e)
            continue


def build_glossary(llm: AzureChatOpenAI, input_path: str, output_path: str) -> None:
    # with open("/Users/asgupta/code/smojol/out/report/V7525186.report/data_structures/V7525186-data.json", 'r') as input_file:
    with open(input_path, 'r') as input_file:
        data_structures = json.load(input_file)
        all_paths: list[list[str]] = []
        collect(data_structures["children"][0], [], llm, all_paths)
        all_explanations = explain_once(all_paths, llm)
        with open(output_path, 'w') as output_file:
            for explanation in all_explanations:
                output_file.write(explanation)
        # print(all_paths, llm)
        exit(0)


if __name__ == "__main__":
    open_ai_endpoint, open_ai_key = openai_config()
    deployment = os.environ.get("LLM_MODEL_LARGE_CONTEXT")

    parser = argparse.ArgumentParser(prog="build_glossary")
    parser.add_argument(ParameterConstants.DS_INPUT_PATH)
    parser.add_argument(ParameterConstants.GLOSSARY_OUTPUT_PATH)
    args = parser.parse_args()
    input_path = args.input_path
    output_path = args.output_path
    report_dir = args.report_dir
    llm4 = AzureChatOpenAI(
        deployment_name=deployment,
        azure_endpoint=open_ai_endpoint,
        openai_api_version="2023-06-01-preview",
        openai_api_key=open_ai_key,
        temperature=0.8,
        request_timeout=6000
    )
    build_glossary(llm4, input_path, output_path)
