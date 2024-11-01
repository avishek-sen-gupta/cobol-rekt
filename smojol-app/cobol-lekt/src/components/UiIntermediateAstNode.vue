<script>

import {defineComponent} from "vue";

export default defineComponent({
      name: "UiIntermediateAstNode",
      props: {
        node: {
          type: Object,
          required: true
        },
        depth: {
          type: Number,
          required: true
        }
      },
      computed: {
        nodeID() {
          return "ast_" + this.node.id;
        },
        isTop() {
          return this.node.nodeType === "TOP";
        },
        isBottom() {
          return this.node.nodeType === "BOTTOM";
        },
        nodeClass() {
          if (this.node.nodeType === "TOP") return "ast_TOP";
          else if (this.node.nodeType === "BOTTOM") return "ast_BOTTOM";
          return "ast_DEFAULT";
        },
        hasChildren() {
          return this.node.childTranspilerNodes.length > 0;
        },
        isLabelledCodeBlock() {
          return this.node.nodeType === "LabelledTranspilerCodeBlockNode";
        },
        isDetachedCodeBlock() {
          return this.node.nodeType === "DetachedTranspilerCodeBlockNode";
        },
        isCodeBlock() {
          return this.node.nodeType === "TranspilerCodeBlockNode";
        },
        isIfBlock() {
          return this.node.nodeType === "IfTranspilerNode";
        },
        isGreaterThan() {
          return this.node.nodeType === "GreaterThanNode";
        },
        isGreaterThanOrEqualTo() {
          return this.node.nodeType === "GreaterThanOrEqualToNode";
        },
        isLessThan() {
          return this.node.nodeType === "LessThanNode";
        },
        isLessThanOrEqualTo() {
          return this.node.nodeType === "LessThanOrEqualToNode";
        },
        isSymbolReference() {
          return this.node.nodeType === "SymbolReferenceNode";
        },
        isValueOf() {
          return this.node.nodeType === "ValueOfNode";
        },
        isPrimitiveReference() {
          return this.node.nodeType === "PrimitiveValueTranspilerNode";
        },
        isNot() {
          return this.node.nodeType === "NotTranspilerNode";
        },
        isPrint() {
          return this.node.nodeType === "PrintTranspilerNode";
        },
        isJump() {
          return this.node.nodeType === "JumpTranspilerNode";
        },
        isPlaceholder() {
          return this.node.nodeType === "PlaceholderTranspilerNode";
        },
        isJumpIf() {
          return this.node.nodeType === "JumpIfTranspilerNode";
        },
        isOr() {
          return this.node.nodeType === "OrTranspilerNode";
        },
        isAnd() {
          return this.node.nodeType === "AndTranspilerNode";
        },
        isEqualTo() {
          return this.node.nodeType === "EqualToNode";
        },
        nextLevel() {
          return this.depth + 1;
        },
        indent() {
          return "  ".repeat(this.depth);
        },
        blockBackgroundColour() {
          const depth = this.depth + 1;
          return {
            backgroundColor: `rgb(${255 - depth * 25}, ${255 - depth * 15}, ${255 - depth * 10})`,
            transform: `translateX(${depth}px)`
          }
          // return {r: 255 - this.depth * 10, g: 255 - this.depth * 20, b: 255 - this.depth * 30};
        }
      }
    }
)
</script>

<template>
  <div :id="nodeID" v-if="isLabelledCodeBlock" :style="blockBackgroundColour">
    <div>{{ indent }}{ <span class="block-name">{{ this.node.name }}</span></div>
    <UiIntermediateAstNode
        v-for="child in node.childTranspilerNodes"
        :key="child.id"
        :node="child"
        :depth="nextLevel"
    />
    <div>{{ indent }}}</div>
  </div>
  <div :id="nodeID" v-else-if="isCodeBlock" :style="blockBackgroundColour">
    <div>{{ indent }}{</div>
    <UiIntermediateAstNode
        v-for="child in node.childTranspilerNodes"
        :key="child.id"
        :node="child"
        :depth="nextLevel"
    />
    <div>{{ indent }}}</div>
  </div>
  <div :id="nodeID" v-else-if="isDetachedCodeBlock" :style="blockBackgroundColour">
    <div>{{ indent }}{</div>
    <UiIntermediateAstNode
        v-for="child in node.childTranspilerNodes"
        :key="child.id"
        :node="child"
        :depth="nextLevel"
    />
    <div>{{ indent }}}</div>
  </div>
  <span :id="nodeID" v-else-if="isSymbolReference">ref("{{ this.node.name }}")
  </span>
  <span :id="nodeID" v-else-if="isValueOf">valueOf(
    <UiIntermediateAstNode
        :node="node.expression"
        :depth="depth"
    />
    )
  </span>
  <span :id="nodeID" v-else-if="isNot">not(
    <UiIntermediateAstNode
        :node="node.expression"
        :depth="depth"
    />
    )
  </span>
  <div :id="nodeID" v-else-if="isPrint">
    {{ indent }}print(
    <UiIntermediateAstNode
        v-for="child in node.operands"
        :key="child.id"
        :node="child"
        :depth="depth"
    />
    ...
    )
  </div>
  <div :id="nodeID" v-else-if="isJump">
    {{ indent }}<span class="jump">jump(
    <UiIntermediateAstNode
        :node="node.start"
        :depth="depth"
    />
    , [not yet implemented]
    )
    </span>
  </div>
  <span :id="nodeID" v-else-if="isJumpIf">
    jumpIf(
    {{ indent }}<UiIntermediateAstNode
      :node="node.destination"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.condition"
        :depth="depth"
    />
    )
  </span>
  <div :id="nodeID" v-else-if="isPlaceholder">
    {{ indent }}...
  </div>
  <span :id="nodeID" v-else-if="isPrimitiveReference">prim({{ this.node.value.value }})
  </span>
  <span :id="nodeID" v-else-if="isOr">or(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isAnd">and(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isEqualTo">eq(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isGreaterThan">gt(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isGreaterThanOrEqualTo">gte(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isLessThan">lt(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isLessThanOrEqualTo">lte(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <div :id="nodeID" v-else-if="isIfBlock">
    {{ indent }}if (
    <UiIntermediateAstNode
        :node="node.condition"
        :depth="depth"
    />
    )
    <UiIntermediateAstNode
        :node="node.ifThenBlock"
        :depth="depth"
    />
    {{ indent }}else
    <UiIntermediateAstNode
        :node="node.ifElseBlock"
        :depth="depth"
    />
  </div>
  <span :id="nodeID" v-else :class="{ast_TOP: isTop, ast_BOTTOM: isBottom}">({{ node.nodeType }}
    <UiIntermediateAstNode
        v-for="child in node.childTranspilerNodes"
        :key="child.id"
        :node="child"
        :depth="nextLevel"
    />
  </span>
</template>

<style scoped>
.ast_TOP {
  background-color: lightpink;
}

.ast_BOTTOM {
  background-color: greenyellow;
}

.block-name {
  background-color: greenyellow;
  border: solid 2px;
}

.jump {
  background-color: gold;
}
</style>
