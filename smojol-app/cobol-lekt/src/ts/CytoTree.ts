import {CytoEdge, CytoModel, CytoNode} from "@/ts/CytoscapeTypes";

type ChildrenAccess = (node: TreeModelNode) => TreeModelNode[];

export type TreeModelNode = { id: string; childTranspilerNodes: TreeModelNode[], [key: string]: any };

export function cytoTreeNodes(current: TreeModelNode, childrenAccess: ChildrenAccess): CytoNode[] {
    const currentGraphNodes: CytoNode[] = [{data: current}];
    if (childrenAccess(current).length === 0) {
        return currentGraphNodes;
    }
    return currentGraphNodes.concat(childrenAccess(current).flatMap((e: TreeModelNode) => cytoTreeNodes(e, childrenAccess)));
}

export function cytoTreeEdges(current: TreeModelNode, thread: TreeModelNode[], childrenAccess: ChildrenAccess): CytoEdge[] {
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
    return myEdges.concat(childrenAccess(current).flatMap((e: TreeModelNode) => cytoTreeEdges(e, thread.concat(current), childrenAccess)));
}

export function asCytoscapeTree(current: TreeModelNode, childrenAccess: ChildrenAccess): (CytoModel) {
    // if (current.childTranspilerNodes === null) return [];
    if (childrenAccess(current) === null) return [];
    const cNodes = cytoTreeNodes(current, childrenAccess);
    const cEdges = cytoTreeEdges(current, [], childrenAccess);
    return cNodes.concat(cEdges);
}

export const TranspilerNodeChildrenAccess: ChildrenAccess = (node)=> node.childTranspilerNodes;
