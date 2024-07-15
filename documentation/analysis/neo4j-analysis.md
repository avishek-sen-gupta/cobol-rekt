You can find data structures and the code which accesses/modifies them using the following:

```
match (n)-[r:MODIFIES|ACCESSES]-(d) RETURN n,r,d
```

For pathfinding algorithms, you will need to create an undirected projection of the graph. Do it one of two ways:

- Create the undirected projection directly. Add all the relations you want to make undirected. The example below shows creating an undirected projection for an AST, which only contains the ```CONTAINS``` relation.

```
CALL gds.graph.project(
  'myGraph',
  ['AST_NODE'],  // Node labels
  {
    CONTAINS: {
      type: 'CONTAINS',
      orientation: 'UNDIRECTED'
    }
  }
)
```

or

```
CALL gds.graph.project(
  'cfgUndirected',
  ['CFG_NODE'],  // Node labels
  {
    FOLLOWED_BY: {
      type: 'FOLLOWED_BY',
      orientation: 'UNDIRECTED'
    },
    STARTS_WITH: {
      type: 'STARTS_WITH',
      orientation: 'UNDIRECTED'
    },
    JUMPS_TO: {
      type: 'JUMPS_TO',
      orientation: 'NATURAL'
    }
  }
)
```

- Mutate existing relationships to undirected on an existing projected graph. You will need to do this for all the relations you want to modify. The example below shows the query for modifying the ```JUMPS_TO``` relation.

```
CALL gds.graph.relationships.toUndirected(
  'myGraph',
    {relationshipType: 'JUMPS_TO', mutateRelationshipType: 'JUMPS_TO_2'}
)
YIELD
  inputRelationships, relationshipsWritten
```

To find a path between any two code nodes (this will show actual execution paths, as well as unrelated nodes which still share a common join point, upstream or downstream):

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

```
// match (source:CFG_NODE)-[r0:MODIFIES]-(d0) match (target:CFG_NODE)-[r1:ACCESSES]-(d0)
MATCH (source:CFG_NODE {internal_id: 'cc9cab97-84f6-46cc-8f2a-cbf4d9d956f9'}), (target:CFG_NODE {internal_id: '69ee5841-ae10-406a-85b8-bf8fd875fb65'})
CALL gds.shortestPath.dijkstra.stream('myGraph', {
    sourceNode: source,
    targetNodes: target
})
YIELD index, sourceNode, targetNode, totalCost, nodeIds, costs, path
RETURN
    // d0,
    index,
    gds.util.asNode(sourceNode).name AS sourceNodeName,
    gds.util.asNode(targetNode).name AS targetNodeName,
    totalCost,
    [nodeId IN nodeIds | gds.util.asNode(nodeId).name] AS nodeNames,
    costs,
    nodes(path) as path
ORDER BY index
```

To find the common ancestor code node of two code nodes, use the following query template.

```
MATCH (c1:AST_NODE {internal_id: 'CODE_NODE_1_ID'})
MATCH (c2:AST_NODE {internal_id: 'CODE_NODE_2_ID'})

MATCH path = (c1)<-[:CONTAINS*1..5]-(p:AST_NODE)-[:CONTAINS*1..5]-(c2)
RETURN path
ORDER BY length(path)
LIMIT 1
```

- Delete all nodes like so:

```
MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r
```
