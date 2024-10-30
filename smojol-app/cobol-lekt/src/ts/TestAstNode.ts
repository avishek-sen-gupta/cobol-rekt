export class TestAstNode {
    public id: String;
    public children: TestAstNode[];
    public nodeType: String;

    constructor(id: String, nodeType: String, children: TestAstNode[]) {
        this.id = id;
        this.children = children;
        this.nodeType = nodeType;
    }
}
