import sys
from functools import reduce
from typing import Callable, Iterable

import matplotlib.pyplot as plt
import numpy as np
from gensim.models import KeyedVectors
from langchain_community.graphs import Neo4jGraph
from langchain_openai import AzureChatOpenAI
from neo4j import GraphDatabase, Driver
from scipy.cluster.hierarchy import linkage, dendrogram
from scipy.spatial.distance import pdist
from tqdm import tqdm

from src.llm.capability_extractor.domain_cluster import DomainCluster
from src.llm.common.console_colors import ConsoleColors
from src.llm.common.env_vars import openai_config, neo4j_config

c = ConsoleColors()


def recurse_to_graph(cluster: DomainCluster, parent: DomainCluster | None,
                     neo4j_runtime_config: tuple[Driver, str]) -> None:
    driver, database = neo4j_runtime_config
    print(f".", end="")
    composite_type = "COMPOSITE" if cluster.composite else "LEAF"
    if parent is not None:
        driver.execute_query(
            "MATCH (p:DOMAIN {index: $parent_index})\n" +
            f"CREATE (n:DOMAIN:{composite_type}"
            + " {isComposite: $is_composite, index: $index, content: $content, domain: $domain, umbrella_domain: $umbrella_subdomain})\n"
            + """
            MERGE (p)-[r:CONTAINS]->(n)
            RETURN p,n,r
            """, {"is_composite": cluster.composite,
                  "index": cluster.index,
                  "parent_index": parent.index,
                  "domain": cluster.domain,
                  "composite_type": composite_type,
                  "umbrella_subdomain": cluster.umbrella_subdomain,
                  "content": cluster.content}, database=database)
    else:
        driver.execute_query("""
            CREATE (n:DOMAIN:COMPOSITE {isComposite: $is_composite, index: $index, content: $content, domain: $domain, umbrella_domain: $umbrella_subdomain})
            RETURN n
        """, {"is_composite": cluster.composite,
              "index": cluster.index,
              "parent_index": "-1",
              "domain": cluster.domain,
              "composite_type": composite_type,
              "umbrella_subdomain": cluster.umbrella_subdomain,
              "content": cluster.content}, database=database)
    for child in cluster.children:
        recurse_to_graph(child, cluster, neo4j_runtime_config)


def recurse_for_domain(cluster: DomainCluster, llm: AzureChatOpenAI) -> None:
    if not cluster.composite:
        cluster.umbrella_subdomain = cluster.domain
        return
    for child in cluster.children:
        recurse_for_domain(child, llm)
    child_domains: list[str] = list(map(lambda c: c.domain, cluster.children))
    domains_as_string: str = reduce(lambda acc, word: acc + "," + word, child_domains, "")
    cluster.domain = domains_as_string
    cluster.umbrella_subdomain = umbrella_term(cluster, llm)
    # cluster.umbrella_subdomain = "DUMMY"


def umbrella_term(cluster: DomainCluster, llm: AzureChatOpenAI) -> str:
    child_domains: list[str] = list(map(lambda c: c.domain, cluster.children))
    domains_as_string: str = reduce(lambda acc, word: acc + "," + word, child_domains, "")
    child_umbrella_subdomains: list[str] = list(map(lambda c: c.umbrella_subdomain, cluster.children))
    umbrella_domains_as_string: str = reduce(lambda acc, word: acc + "," + word, child_umbrella_subdomains, "")
    system_message = f"""
            You are an expert of Mainframe and IDMS codebases.
            The user will provide you with a list of sub-domains, as well as the sub-sub-domains
            they are derived from. These sub-domains are associated with the code of a
            mainframe system dealing with after-sales operations, procurement, and logistics of a German automotive company.
            Your task is to respond with only 1 or 2 (NOT MORE) of descriptive umbrella terms, delimited by commas,
            which encompass all of these sub-domains. Do not output any extra text apart from the umbrella domain term.
            The word "After-sales" or its variations should only occur as part of the term, and not be the whole term.
            The sub-sub-domains are: [{domains_as_string}]
            The child umbrella terms are: [{umbrella_domains_as_string}]
    """
    while True:
        try:
            response = llm.invoke(system_message)
            # print(c.grey(response.content))
            print(c.grey("."), end="")
            return response.content
        except Exception as e:
            print(c.fail(e))
            continue


def attach_paragraphs(pairs: list[tuple[str, list[str]]], neo4j_runtime_config: tuple[Driver, str]):
    driver, database = neo4j_runtime_config
    for p in tqdm(pairs, colour="green"):
        domains: list[str] = p[1]
        driver.execute_query("""
            CREATE (n:CODE {name: $name})
            RETURN n
        """, {"name": p[0]}, database=database)

        for domain in domains:
            driver.execute_query("""
                MATCH (d:DOMAIN {domain: $domain})
                MATCH (n:CODE {name: $name})
                MERGE (n)-[r:REPRESENTS]->(d)
                RETURN n
            """, {"name": p[0],
                  "domain": domain.lower().strip()}, database=database)

    pass


if __name__ == "__main__":
    print(sys.version)
    open_ai_endpoint, open_ai_key = openai_config()
    neo4j_uri, neo4j_username, neo4j_password, neo4j_database = neo4j_config()

    auth = (neo4j_username, neo4j_password)
    llm4 = AzureChatOpenAI(
        deployment_name="gpt-4o",
        azure_endpoint=open_ai_endpoint,
        openai_api_version="2023-06-01-preview",
        openai_api_key=open_ai_key,
        temperature=0.8,
        request_timeout=6000
    )
    graph = Neo4jGraph(neo4j_uri, username=neo4j_username, password=neo4j_password)
    with open("/Users/asgupta/code/smojol-llm/artifacts/capabilities2.txt", "r") as fp:
        lines: list[str] = fp.readlines()
    pairs = list(map(lambda line: (
        line.split(":")[0].replace("'", " "), list(map(lambda term: term.strip(), line.split(":")[1].split(",")))),
                     lines))

    all_terms: list[str] = list(reduce(lambda full, pair: full + pair[1], pairs, []))
    unique_terms = list(map(lambda w: w.lower().strip(), set(all_terms)))

    print(unique_terms)

    # Load pre-trained word2vec model (Google News 300)
    model_path = '/Users/asgupta/Downloads/google-news-embeddings/GoogleNews-vectors-negative300.bin'
    model = KeyedVectors.load_word2vec_format(model_path, binary=True)
    words = unique_terms
    words = [word for word in words if word in model]
    word_vectors = np.array([model[word] for word in words])
    distances = pdist(word_vectors, metric='cosine')

    # Perform hierarchical/agglomerative clustering
    linkage_matrix = linkage(distances, method='ward')
    n = len(words)
    nodes: dict[int, DomainCluster] = {}
    for index, merge in enumerate(tqdm(linkage_matrix, colour="green")):
        left_index = int(merge[0])
        right_index = int(merge[1])
        left_cluster = nodes[left_index] if left_index in nodes else DomainCluster(left_index, words)
        right_cluster = nodes[right_index] if right_index in nodes else DomainCluster(right_index, words)
        nodes[left_index] = left_cluster
        nodes[right_index] = right_cluster
        parent_cluster = DomainCluster(n + index, words, [left_cluster, right_cluster])
        nodes[n + index] = parent_cluster

    print("\nBuilding domain...")
    recurse_for_domain(parent_cluster, llm4)
    with GraphDatabase.driver(neo4j_uri, auth=auth) as driver:
        neo4j_runtime_config = (driver, neo4j_database)
        print("\nBuilding Neo4J model...")
        recurse_to_graph(parent_cluster, None, neo4j_runtime_config)
        print("\nAttaching paragraphs to leaf domains...\n")
        attach_paragraphs(pairs, neo4j_runtime_config)
    # Plot the dendrogram
    plt.figure(figsize=(10, 5))
    dendrogram(linkage_matrix, labels=words)
    plt.title('Hierarchical Clustering Dendrogram')
    plt.xlabel('Words')
    plt.ylabel('Distance')
    plt.show()
