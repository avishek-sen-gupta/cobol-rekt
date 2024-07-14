You can find data structures and the code which accesses/modifies them using the following:

```match (n0)-[r0:MODIFIES]-(d0) match (n1)-[r1:ACCESSES]-(d0) RETURN n0,n1,r0,r1,d0```

For pathfinding algorithms, you will need to create an undirected projection of the graph:

```
CALL gds.graph.relationships.toUndirected(
  'myGraph',
    {relationshipType: 'JUMPS_TO', mutateRelationshipType: 'JUMPS_TO_2'}
)
YIELD
  inputRelationships, relationshipsWritten
```

To find a path between any two code nodes (this will show actual execution, as well as unrelated nodes which still share a common join point, upstream or downstream):

```
MATCH (source:CFG_NODE {internal_id: 'source-internal-id'}), (target:CFG_NODE {internal_id: 'target-internal-id'})
CALL gds.shortestPath.dijkstra.stream('myGraph', {
    sourceNode: source,
    targetNodes: target
})
YIELD index, sourceNode, targetNode, totalCost, nodeIds, costs, path
RETURN
    index,
    gds.util.asNode(sourceNode).name AS sourceNodeName,
    gds.util.asNode(targetNode).name AS targetNodeName,
    totalCost,
    [nodeId IN nodeIds | gds.util.asNode(nodeId).name] AS nodeNames,
    costs,
    nodes(path) as path
ORDER BY index
```

