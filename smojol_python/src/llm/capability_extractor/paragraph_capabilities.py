import argparse
import json

from dotenv import load_dotenv
from langchain_openai import AzureChatOpenAI
from neo4j import GraphDatabase, Record

from src.llm.capability_extractor.CacheStats import CacheStats
from src.llm.common.console_colors import ConsoleColors
from src.llm.common.env_vars import openai_config, neo4j_config
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)

c = ConsoleColors()


def collect(struct, path: list[str], llm: AzureChatOpenAI, all_paths: list[list[str]]) -> None:
    if "ROOT" not in struct["name"]:
        all_paths += [path + [struct["name"]]]
        # explain_through_llm(struct, path, llm)
    for child in struct["children"]:
        collect(child, path + [struct["name"]], llm, all_paths)


def matching_structures(structure_name: str, all_paths: list[list[str]]) -> tuple[str, list[list[str]]]:
    return structure_name, [path for path in all_paths if path[-1] == structure_name]


def llm_explanation_block(paragraph_name: str, explanation: str, llm: AzureChatOpenAI) -> str:
    system_message = f"""
                        You are an expert of Mainframe and IDMS codebases.
                        The user will provide you with the name of a COBOL paragraph as well as an explanation of all the
                        variables that are modified or accesses in this paragraph.
                        This paragraph is part of a larger program in the automotive after-sales domain of a German car manufacturing company.
                        You need to return a set of terms related to the domain of after-sales car parts supply and procurement.
                        These terms need to be English. Examples of terms are 'Spare Part Invoices', 'Invoice Correction', 'Inbound/Outbound Sales'. 'Spare Parts Checks', 'Billing', etc.
                        Do not blindly use the above terms unless you are sure that is what is indicated. Take into account the alternative expansions of the variables as well, when deciding what terms to assign.
                        Always make sure that these terms reflect the domain of the automotive after-sales industry.
                        Give me the response as ONLY a comma separated list of words. Do not add any extraneous output.
                        Examples of outputs are: ```sales, billing, taxes``` and ```dog, cat, bird```.
                        Restrict your response to no more than 10 domain concepts. Be as brief as possible in your response.
                        The paragraph name is {paragraph_name}. The explanation of the variables follows below.\n '{explanation}'\n
                            """
    while True:
        try:
            response = llm.invoke(system_message)
            print(c.grey(str(response.content)))
            return str(response.content)
        except Exception as e:
            print(c.fail(e))
            continue


def all_capabilities(ds_input_path: str, paragrah_capabilities_output_path: str, neo4j_uri: str, auth: tuple[str, str],
                     llm: AzureChatOpenAI, database: str):
    master_explanations: dict[str, str] = {}
    cache_stats = CacheStats()
    all_paths = data_structure_graph(llm, ds_input_path)
    with GraphDatabase.driver(neo4j_uri, auth=auth) as driver:
        result = driver.execute_query("""
            match (n:PARAGRAPH)
            RETURN n
        """, database_=database)
        paragraph_ids = list(map(lambda pnode: (pnode["n"]["internal_id"], pnode["n"]["name"]), result[0]))
        paragraphs_with_capabilities = {}

        for idx, pid in enumerate(paragraph_ids):
            overall_progress = f"{pid[1]} : ({idx + 1}/{len(paragraph_ids)})"
            print(c.cyan(f"Extracting capabilities for {overall_progress} [num_cache_hits = {cache_stats.hits}]..."))
            internal_id = pid[0]
            result = driver.execute_query("""
                match (n:PARAGRAPH{internal_id: $paragraph_id})-[r*1..6]->(d:DATA_STRUCTURE) 
                RETURN n,r,d
            """, {"paragraph_id": internal_id}, database_=database)
            if len(result[0]) == 0:
                print(c.warning(f"No data nodes found for {pid[1]}, skipping to next paragraph..."))
                continue
            paragraphs_with_capabilities[pid[1]] = capabilities_for_paragraph(result[0], all_paths, master_explanations,
                                                                              overall_progress, llm,
                                                                              cache_stats)
            f = open(paragrah_capabilities_output_path, "a")
            f.write(f"'{pid[1]}':{paragraphs_with_capabilities[pid[1]]}\n")
            f.close()

    return paragraphs_with_capabilities, master_explanations


def capabilities_for_paragraph(nrds: list[Record], all_paths: list[list[str]], master_explanations: dict,
                               overall_progress: str,
                               llm: AzureChatOpenAI, cache_stats: CacheStats) -> str:
    if len(nrds) == 0:
        return ""
    explanation = ""
    explanation_map = set()
    for nrd in nrds:
        explanation_map.add(nrd["d"])
    for i, dataNode in enumerate(explanation_map):
        print(c.green(
            f"Explaining variable {dataNode['name']} ({i + 1}/{len(explanation_map)}) [Overall progress = {overall_progress}, num_cache_hits = {cache_stats.hits}]..."))
        structure_name, structure_paths = matching_structures(dataNode["name"], all_paths)
        if structure_name == "FILLER":
            print(c.warning("Skipping FILLER..."))
            continue
        if len(structure_paths) == 0:
            print(c.warning("Skipping because path is empty, so probably a non-pertinent static constant..."))
            continue
        if structure_name in master_explanations:
            print(c.warning(
                f"Explanation of variable {structure_name} already exists, using existing explanation:\n {master_explanations[structure_name]}"))
            explanation += f"This is the domain relevance of {structure_name}: \n" + master_explanations[
                structure_name] + "\n"
            cache_stats.add()
            continue
        generated_explanation = llm_explanation(structure_name, structure_paths[0], llm)
        master_explanations[structure_name] = generated_explanation
        explanation += f"This is the domain relevance of {structure_name}: \n" + generated_explanation + "\n"

    return llm_explanation_block(nrds[0]["n"]["name"], explanation, llm)


def data_structure_graph(llm: AzureChatOpenAI, ds_input_path: str) -> list[list[str]]:
    with open(ds_input_path, 'r') as file:
        data_structures = json.load(file)
        all_paths: list[list[str]] = []
        collect(data_structures["children"][0], [], llm, all_paths)
        return all_paths


def llm_explanation(struct_name: str, path: list[str], llm: AzureChatOpenAI) -> str:
    print(c.bold(f"Path is {path} for {struct_name}"))
    system_message = f"""
                                You are an expert of Mainframe and IDMS codebases.
                                The user will provide you with the name of a data structure as well as the hierarchy of that variable
                                in a list. This data structure exists in the automotive after-sales domain of a German company.
                                The code is written in a mix of Cobol and the IDMS dialect, so there will be mainframe terms mixed in throughout.
                                Some variables may also have German words; translate them into English words as needed.
                                Your task is to explain the expansion of the name of the data structure.
                                Expand the name of the data structure with the name: '{struct_name}'. Suggest alternative expansions as well. Consider the context as well when interpreting the variable name. Ignore variable names
                                like 'FILLER' and 'ROOT'. They are placeholders, so don't expand on them, or explain them. Also, in your responses, be as concise (but not unhelpful) and professional as possible.
                                The hierarchy is '{path}'
                            """
    while True:
        try:
            response = llm.invoke(system_message)
            print(c.grey(response.content))
            return str(response.content)
        except Exception as e:
            print(c.fail(e))
            continue


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="build_paragraph_capabilities")
    parser.add_argument(ParameterConstants.DS_INPUT_PATH)
    parser.add_argument(ParameterConstants.PARAGRAPH_CAPABILITIES_OUTPUT_PATH)
    parser.add_argument(ParameterConstants.PARAGRAPH_VARIABLES_EXPLANATIONS_OUTPUT_PATH)
    args = parser.parse_args()
    data_structures_input_path = getattr(args, ParameterConstants.DS_INPUT_PATH)
    paragrah_capabilities_output_path = getattr(args, ParameterConstants.PARAGRAPH_CAPABILITIES_OUTPUT_PATH)
    paragraph_variables_explanations_output_path = (
        getattr(args, ParameterConstants.PARAGRAPH_VARIABLES_EXPLANATIONS_OUTPUT_PATH))

    open_ai_endpoint, open_ai_key = openai_config()
    neo4j_uri, neo4j_username, neo4j_password, neo4j_database = neo4j_config()
    print(c.green(open_ai_endpoint))
    print(c.green(neo4j_uri))

    auth = (neo4j_username, neo4j_password)
    llm4 = AzureChatOpenAI(
        deployment_name="gpt-4o",
        azure_endpoint=open_ai_endpoint,
        openai_api_version="2023-06-01-preview",
        openai_api_key=open_ai_key,
        temperature=0.8,
        request_timeout=6000
    )

    f = open(paragrah_capabilities_output_path, "w")
    f.write("")
    f.close()
    capabilities, variable_explanations = all_capabilities(data_structures_input_path,
                                                           paragrah_capabilities_output_path, neo4j_uri, auth, llm4, neo4j_database)
    with open(paragraph_variables_explanations_output_path, "w") as fp:
        json.dump(variable_explanations, fp)
    print(c.cyan(f"Capabilities are: {capabilities}"))
