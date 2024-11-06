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
    const unifiedModelCodeVertices: DigraphNode[] = model.codeVertices.map(cv => ({
        id: cv.id,
        label: cv.label,
        type: cv.type
    }));
    const unifiedModelDataVertices: DigraphNode[] = model.dataVertices.map(dv => ({
        id: dv.id,
        name: dv.name,
        type: dv.dataType
    }));
    const cytoNodes: DigraphNode[] = unifiedModelCodeVertices.concat(unifiedModelDataVertices);
    const edges: DigraphEdge[] = model.edges.map((dge) => ({
        id: dge.id,
        source: {id: dge.fromNodeID},
        target: {id: dge.toNodeID}
    }));
    return {nodes: cytoNodes, edges};
}
