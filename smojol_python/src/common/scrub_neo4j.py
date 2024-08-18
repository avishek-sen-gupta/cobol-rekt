import os

from dotenv import load_dotenv
from neo4j import GraphDatabase


def scrub_neo4j(neo4j_credentials: tuple[str, tuple[str, str], str]):
    neo4j_uri, neo4j_auth, database = neo4j_credentials
    with GraphDatabase.driver(neo4j_uri, auth=neo4j_auth) as driver:
        driver.execute_query(
            """MATCH(n) DETACH DELETE n""",
            {},
            database_=database,
        )


if __name__ == "__main__":
    load_dotenv("env/.env", override=True)
    neo4j_uri = os.environ.get("NEO4J_URI")
    neo4j_username = os.environ.get("NEO4J_USERNAME")
    neo4j_password = os.environ.get("NEO4J_PASSWORD")
    neo4j_database = os.environ.get("NEO4J_DATABASE", "neo4j")
    scrub_neo4j((neo4j_uri, (neo4j_username, neo4j_password), neo4j_database))
