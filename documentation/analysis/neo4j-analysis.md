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
MATCH (source:CFG_NODE {internal_id: 'CODE_NODE_1_ID'}), (target:CFG_NODE {internal_id: 'CODE_NODE_2_ID'})
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

This gets all data influencers as well as the entire data layout (can take a while to run).

```
match (a:CFG_NODE)-[b:MODIFIES|ACCESSES]-(c) match (n:DATA_STRUCTURE)-[r]-(d:DATA_STRUCTURE) RETURN a,b,c,n,r,d
```

All unused variable trees

```
MATCH (parent)-[:CONTAINS*]->(descendant)
WITH parent, COLLECT(descendant) AS descendants
WHERE ALL(d IN descendants WHERE NOT (d)<-[:MODIFIES|ACCESSES]-())
RETURN parent,descendants
```
