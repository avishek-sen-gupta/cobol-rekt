from __future__ import annotations

import networkx as nx
from matplotlib import pyplot as plt

if __name__ == "__main__":
    graphml = nx.read_graphml("/Users/asgupta/code/smojol/out/report/test-exp.cbl.report/graphml/test-exp.cbl.graphml")
    print(list(map(lambda n: n, list(graphml.nodes))))
    # print(list(subgraph1.nodes))

    pos = nx.kamada_kawai_layout(graphml)
    nx.draw(graphml, pos, with_labels=False)
    plt.show()
    # print(paragraphs)
