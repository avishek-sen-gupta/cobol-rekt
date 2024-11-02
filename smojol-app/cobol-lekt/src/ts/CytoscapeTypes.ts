export type CytoEdge = { data: { id: string, source: string, target: string }; }
export type CytoNode = { data: { id: string, [key: string]: any } }
export type CytoModel = (CytoNode | CytoEdge)[];
