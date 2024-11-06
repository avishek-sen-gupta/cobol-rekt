import {CytoEdge, CytoModel, CytoNode} from "@/ts/CytoscapeTypes";

export type DigraphNode = { id: string, [key: string]: any };
export type DigraphEdge = { source: DigraphNode, target: DigraphNode, [key: string]: any };
export type Digraph = { nodes: DigraphNode[], edges: DigraphEdge[] };

export function cytoDigraphNodes(graph: Digraph): CytoNode[] {
    return graph.nodes.map((dgn) => {
        return {data: {id: dgn.id, dgn}};
    });
}

export function cytoDigraphEdges(graph: Digraph): CytoEdge[] {
    return graph.edges.map((dge) => {
        return {
            data: {
                id: dge.source.id + dge.target.id,
                source: dge.source.id,
                target: dge.target.id
            }
        };
    });
}

export function asCytoscapeDigraph(graph: Digraph): (CytoModel) {
    const cNodes = cytoDigraphNodes(graph);
    const cEdges = cytoDigraphEdges(graph);
    return cNodes.concat(cEdges);
}
