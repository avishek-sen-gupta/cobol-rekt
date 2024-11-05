export type MutableCenter = {id: string, flip: boolean};

export function flip(id: string, node: MutableCenter): MutableCenter {
    return {id, flip: node ? !node.flip : true};
}
