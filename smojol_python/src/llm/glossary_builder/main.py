import json

from langchain.chains import GraphCypherQAChain
from langchain_community.graphs import Neo4jGraph
from langchain_openai import AzureChatOpenAI

from src.llm.common.env_vars import openai_config, neo4j_config


def explain_through_llm(struct, path, llm):
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


def collect(struct, path, llm, all_paths):
    if "ROOT" not in struct["name"]:
        all_paths += [path + [struct["name"]]]
        # explain_through_llm(struct, path, llm)
    for child in struct["children"]:
        collect(child, path + [struct["name"]], llm, all_paths)


def explain_once(all_paths, llm):
    print(f"Explaining {len(all_paths)} data structures")
    for x in range(0, len(all_paths), 20):
        explain_batch(all_paths[x:(x + 19)], llm, x, len(all_paths))


def explain_batch(all_paths, llm, index, max):
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
            break
        except Exception as e:
            print(e)
            continue


def build_glossary(llm):
    with open("/Users/asgupta/code/smojol/out/report/V7525186.report/data_structures/V7525186-data.json", 'r') as file:
        data_structures = json.load(file)
        all_paths = []
        collect(data_structures["children"][0], [], llm, all_paths)
        explain_once(all_paths, llm)
        # print(all_paths, llm)
        exit(0)


if __name__ == "__main__":
    open_ai_endpoint, open_ai_key = openai_config()
    neo4j_uri, neo4j_username, neo4j_password = neo4j_config()

    llm4 = AzureChatOpenAI(
        deployment_name="gpt-4o",
        azure_endpoint=open_ai_endpoint,
        openai_api_version="2023-06-01-preview",
        openai_api_key=open_ai_key,
        temperature=0.8,
        request_timeout=6000
    )
    build_glossary(llm4)

    graph = Neo4jGraph(neo4j_uri, username=neo4j_username, password=neo4j_password)
    graph.refresh_schema()

    explorerChain = GraphCypherQAChain.from_llm(llm4, graph=graph, verbose=True)

    # while True:
    #     try:
    #         response = explorerChain.invoke("""
    #         'Z-BO-PAMA' is a data structure. Deduce what it represents in the domain, if the domain is automotive after-sales. Return the context surrounding this data structure. Consider all statements
    #         which modify or access this data structure using the 'MODIFIES' and 'ACCESSES' edges, including
    #         neighbouring statements. Check to a minimum of 10 neighbouring statements.Also consider other variables
    #         which affect it using the FLOWS_INTO connection. Also consider any comments attached to neighbouring data
    #         structures with the 'HAS_COMMENT' connection. the domain-specific string terms must be in plain English.
    #         Run multiple queries if needed. Do not return an empty list. Re-run the query if you have to. )
    #         """)
    #         if len(response) == 0:
    #             continue
    #         break
    #     except Exception as e:
    #         print(e)
    #         continue
    #
    # context = response["result"]
    # var_name = "Z-BO-PAMA"
