type CytoEdge = { data: { id: string, source: string, target: string }; }
type CytoNode = { data: { id: string, [key: string]: any } }
type ChildrenAccess = (node: ModelNode) => ModelNode[];

export type ModelNode = { id: string; childTranspilerNodes: ModelNode[], [key: string]: any };

export function cytoNodes(current: ModelNode, childrenAccess: ChildrenAccess): CytoNode[] {
    const currentGraphNodes: CytoNode[] = [{data: current}];
    if (childrenAccess(current).length === 0) {
        return currentGraphNodes;
    }
    return currentGraphNodes.concat(childrenAccess(current).flatMap((e: ModelNode) => cytoNodes(e, childrenAccess)));
}

export function cytoEdges(current: ModelNode, thread: ModelNode[], childrenAccess: ChildrenAccess): CytoEdge[] {
    const parentNode = thread.length === 0 ? null : thread.at(-1);
    const myEdges = parentNode == null ? [] : [{
        data: {
            id: current.id + parentNode.id,
            source: parentNode.id,
            target: current.id
        }
    }];
    if (childrenAccess(current).length == 0) {
        return myEdges;
    }
    return myEdges.concat(childrenAccess(current).flatMap((e: ModelNode) => cytoEdges(e, thread.concat(current), childrenAccess)));
}

export function asCytoscapeModel(current: ModelNode, childrenAccess: ChildrenAccess): ((CytoNode | CytoEdge)[]) {
    // if (current.childTranspilerNodes === null) return [];
    if (childrenAccess(current) === null) return [];
    const cNodes = cytoNodes(current, childrenAccess);
    const cEdges = cytoEdges(current, [], childrenAccess);
    return cNodes.concat(cEdges);
}

export const TranspilerChildrenAccess: ChildrenAccess = (node)=> node.childTranspilerNodes;
