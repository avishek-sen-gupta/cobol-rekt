import gSpan
import torch
from torch_geometric.data import Data

if __name__ == "__main__":
    print("Hello world")
    edges = torch.tensor([[0, 1], [1, 2]], dtype=torch.int)
    nodes = torch.tensor([[1], [2], [3]], dtype=torch.int)
    graph = Data(x=nodes, edge_index=edges)
    gSpan(da)
