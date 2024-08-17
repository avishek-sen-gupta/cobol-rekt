import argparse
import json
from typing import Callable

import pygraphviz

import networkx as nx
from dotenv import load_dotenv
from networkx import MultiDiGraph
from networkx.algorithms.isomorphism import GraphMatcher, DiGraphMatcher

from src.common.analysis_edge import AnalysisEdge
from src.common.flow_node import FlowNode
from src.ingestion.unified_model_to_networkx import extract_ast, extract_cfg, extract_ds
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)


class NodePattern:
    def __init__(self, type: str, id: int):
        self.type = type
        self.id = id


def sequence_pattern_sentence(statement_type: str, number: int):
    subgraph_pattern = MultiDiGraph()
    sentences = list(map(lambda n: NodePattern("SENTENCE", n), range(number)))
    statements = list(map(lambda n: NodePattern(statement_type, n), range(number)))
    for sentence in sentences:
        subgraph_pattern.add_node(sentence, obj=sentence)
    for statement in statements:
        subgraph_pattern.add_node(statement, obj=statement)
    for sntc, stmt in zip(sentences, statements):
        subgraph_pattern.add_edge(sntc, stmt, edge_type="STARTS_WITH")
    sentence_pairs = list(zip(sentences, sentences[1:]))
    # subgraph_pattern.add_edge(statements[0], statements[1], edge_type="LOL")
    for s1, s2 in sentence_pairs:
        subgraph_pattern.add_edge(s1, s2, edge_type="FOLLOWED_BY")
    return subgraph_pattern


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="pattern_matcher")
    parser.add_argument(ParameterConstants.UNIFIED_MODEL_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.UNIFIED_MODEL_PATH)
    with open(input_path, 'r') as file:
        unified = json.load(file)
        ast_x, _, _ = extract_ast(unified)
        cfg_x, _, _ = extract_cfg(unified)
        ds_x, _, _ = extract_ds(unified)
        pos = nx.random_layout(ast_x)
        nx.nx_agraph.write_dot(cfg_x, "/Users/asgupta/code/smojol/out/nxstuff.dot")
        node_match: Callable[[FlowNode, FlowNode], bool] = lambda n1, n2: n1["obj"].type == n2["obj"].type
        edge_match: Callable[[AnalysisEdge, AnalysisEdge], bool] = lambda e1, e2: e1[0]["edge_type"] == e2[0][
            "edge_type"]
        x = sequence_pattern_sentence("MOVE", 18)
        matcher = DiGraphMatcher(cfg_x, x, node_match=node_match, edge_match=edge_match)
        isomorphisms_iter = matcher.subgraph_isomorphisms_iter()
        count = 0
        for i in isomorphisms_iter:
            # print("Found an iso: ")
            # print(i)
            for k, v in i.items():
                print(k.original_text + "/" + v.type)
            # print(count)
            count += 1
        print(f"{count} patterns found.")
        # nx.draw(ast_x, pos, with_labels=False)
        # plt.show()
    # print(paragraphs)
