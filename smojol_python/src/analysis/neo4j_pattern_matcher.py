import os

from dotenv import load_dotenv
from neo4j import GraphDatabase
from neo4j.graph import Path

from src.analysis.neo4j_pattern_queries import call_pattern

load_dotenv("env/.env", override=True)


def match_pattern(neo4j_credentials: tuple[str, tuple[str, str], str]) -> list[Path]:
    neo4j_uri, neo4j_auth, neo4j_database = neo4j_credentials

    with GraphDatabase.driver(neo4j_uri, auth=neo4j_auth) as driver:
        result = driver.execute_query(call_pattern(), {}, database_=neo4j_database)
        return list(map(lambda record: record[0], result[0]))


if __name__ == "__main__":
    neo4j_uri = os.environ.get("NEO4J_URI")
    neo4j_username = os.environ.get("NEO4J_USERNAME")
    neo4j_password = os.environ.get("NEO4J_PASSWORD")
    neo4j_database = os.environ.get("NEO4J_DATABASE", "neo4j")
    paths = match_pattern((neo4j_uri, (neo4j_username, neo4j_password), neo4j_database))
    print("DONE")
