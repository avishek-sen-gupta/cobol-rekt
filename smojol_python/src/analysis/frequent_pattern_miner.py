import argparse
import json

from dotenv import load_dotenv

from src.common.flow_node import FlowNode
from src.gspan_mining.gspan import gSpan
from src.ingestion.unified_model_to_networkx import extract_cfg
from src.llm.common.parameter_constants import ParameterConstants

load_dotenv("env/.env", override=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="frequent_pattern_mining")
    parser.add_argument(ParameterConstants.UNIFIED_MODEL_PATH)
    args = parser.parse_args()
    input_path = getattr(args, ParameterConstants.UNIFIED_MODEL_PATH)
    with open(input_path, 'r') as file:
        unified = json.load(file)
        cfg_x, _, _ = extract_cfg(unified)
        node_integer_mapping: dict[int, FlowNode] = {}
        all_nodes = cfg_x.nodes
        all_edges = cfg_x.edges

        flow_ids = list(map(lambda ix, node: (ix, node.node_category, node), enumerate(all_nodes)))
        for flow_id, node in flow_ids:
            node_integer_mapping[flow_id] = node

        cg = gSpan(
            database_file_name="/Users/asgupta/code/smojol/smojol_python/test_data/simple.txt",
            min_support=1,
            min_num_vertices=8,
            max_num_vertices=15,
            is_undirected=False,
            verbose=True,
            visualize=True
        )

        cg.run()
        cg.time_stats()
        # return cg
        # paths = match_pattern((neo4j_uri, (neo4j_username, neo4j_password), neo4j_database))
        print("DONE")
