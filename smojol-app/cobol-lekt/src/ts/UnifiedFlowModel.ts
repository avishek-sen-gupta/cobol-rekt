import {Digraph, DigraphEdge, DigraphNode} from "@/ts/Digraph";

type UnifiedModelCodeVertex = { id: string, [key: string]: any };
type UnifiedModelDataVertex = { id: string, [key: string]: any };
type UnifiedModelEdge = { id: string, fromNodeID: string, toNodeID: string, [key: string]: any };

type UnifiedModelGraph = {
    codeVertices: UnifiedModelCodeVertex[],
    dataVertices: UnifiedModelDataVertex[],
    edges: UnifiedModelEdge[]
};

export function unifiedModelToDigraph(model: UnifiedModelGraph): Digraph {
    console.log("Not converting yet");
    const cytoNodes: DigraphNode[] = model.codeVertices.concat(model.dataVertices);
    const edges: DigraphEdge[] = model.edges.map((dge) => ({
        id: dge.id,
        source: {id: dge.fromNodeID},
        target: {id: dge.toNodeID}
    }));
    return {nodes: cytoNodes, edges};
}
