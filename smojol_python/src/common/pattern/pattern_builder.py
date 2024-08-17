from typing import Callable

from networkx import MultiDiGraph, Graph
from networkx.algorithms.isomorphism import DiGraphMatcher

from src.common.analysis_edge import AnalysisEdge
from src.common.flow_node import FlowNode
from src.common.pattern.pattern_node import PatternNode


def sentence_sequence_pattern(statement_type: str, number: int, subgraph_pattern=MultiDiGraph()) -> tuple[
    Graph, list[PatternNode]]:
    sentences = list(map(lambda n: sentence_with_single_statement(statement_type, subgraph_pattern)[1], range(number)))
    sentence_pairs = list(zip(sentences, sentences[1:]))
    for s1, s2 in sentence_pairs:
        subgraph_pattern.add_edge(s1, s2, edge_type="FOLLOWED_BY")
    return subgraph_pattern, sentences


def sentence_with_single_statement(statement_type: str, subgraph_pattern: Graph) -> tuple[Graph, PatternNode]:
    sentence = PatternNode("SENTENCE", 1)
    statement = PatternNode(statement_type, 1)

    subgraph_pattern.add_node(sentence, obj=sentence)
    subgraph_pattern.add_node(statement, obj=statement)
    subgraph_pattern.add_edge(sentence, statement, edge_type="STARTS_WITH")
    return subgraph_pattern, sentence


def call_pattern(number: int, subgraph_pattern=MultiDiGraph()):
    _, call_sentence = sentence_with_single_statement("CALL", subgraph_pattern)
    _, move_sentences = sentence_sequence_pattern("MOVE", number, subgraph_pattern)
    subgraph_pattern.add_edge(move_sentences[-1], call_sentence, edge_type="FOLLOWED_BY")
    return subgraph_pattern, move_sentences, call_sentence


def match_pattern(subgraph_pattern: Graph, supergraph: Graph):
    node_match: Callable[[FlowNode, FlowNode], bool] = lambda n1, n2: n1["obj"].type == n2["obj"].type
    edge_match: Callable[[AnalysisEdge, AnalysisEdge], bool] = lambda e1, e2: e1[0]["edge_type"] == e2[0][
        "edge_type"]
    matcher = DiGraphMatcher(supergraph, subgraph_pattern, node_match=node_match, edge_match=edge_match)
    isomorphisms_iter = matcher.subgraph_isomorphisms_iter()
    count = 0
    for i in isomorphisms_iter:
        print("Isomorphism\n---------------")
        for k, v in i.items():
            print(k.original_text + "/" + v.type)
        count += 1
    print(f"{count} patterns found.")
