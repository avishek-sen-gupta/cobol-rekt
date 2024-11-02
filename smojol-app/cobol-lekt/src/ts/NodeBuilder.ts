type CytoEdge = { data: { id: string, source: string, target: string }; }
type CytoNode = { data: { id: string, [key: string]: any } }

export type ModelNode = { id: string; childTranspilerNodes: ModelNode[], [key: string]: any };

export function cytoNodes(current: ModelNode): CytoNode[] {
    const currentGraphNodes: CytoNode[] = [{data: current}];
    if (current.childTranspilerNodes.length === 0) {
        return currentGraphNodes;
    }
    return currentGraphNodes.concat(current.childTranspilerNodes.flatMap((e: ModelNode) => cytoNodes(e)));
}

export function cytoEdges(current: ModelNode, thread: ModelNode[]): CytoEdge[] {
    const parentNode = thread.length === 0 ? null : thread.at(-1);
    const myEdges = parentNode == null ? [] : [{
        data: {
            id: current.id + parentNode.id,
            source: parentNode.id,
            target: current.id
        }
    }];
    if (current.childTranspilerNodes.length == 0) {
        return myEdges;
    }
    return myEdges.concat(current.childTranspilerNodes.flatMap((e: ModelNode) => cytoEdges(e, thread.concat(current))));
}

export function asCytoscapeModel(current: ModelNode): ((CytoNode | CytoEdge)[]) {
    if (current.childTranspilerNodes === null) return [];
    const cNodes = cytoNodes(current);
    const cEdges = cytoEdges(current, []);
    return cNodes.concat(cEdges);
}
