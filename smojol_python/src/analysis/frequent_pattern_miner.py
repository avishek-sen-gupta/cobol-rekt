import os

from src.gspan_mining.gspan import gSpan
from dotenv import load_dotenv

load_dotenv("env/.env", override=True)

if __name__ == "__main__":
    neo4j_uri = os.environ.get("NEO4J_URI")
    neo4j_username = os.environ.get("NEO4J_USERNAME")
    neo4j_password = os.environ.get("NEO4J_PASSWORD")
    neo4j_database = os.environ.get("NEO4J_DATABASE", "neo4j")
    cg = gSpan(
        database_file_name="/Users/asgupta/code/smojol/smojol_python/simple.txt",
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
