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
      methods: {
        sourceElementClicked(sourceElement) {
          this.$emit("sourceNodeClicked", sourceElement);
          // console.log(sourceElement);
        },
        sourceNodeClicked(sourceNode) {
          // console.log("Bubbling up");
          // console.log(sourceNode);
          this.$emit("sourceNodeClicked", sourceNode);
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
        isEqualTo() {
          return this.node.nodeType === "EqualToNode";
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
        isIndexReference() {
          return this.node.nodeType === "IndexReferenceNode";
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
        isSet() {
          return this.node.nodeType === "SetTranspilerNode";
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
        isNestedCondition() {
          return this.node.nodeType === "NestedConditionNode";
        },
        isOr() {
          return this.node.nodeType === "OrTranspilerNode";
        },
        isAnd() {
          return this.node.nodeType === "AndTranspilerNode";
        },
        isExponent() {
          return this.node.nodeType === "ExponentNode";
        },
        isAdd() {
          return this.node.nodeType === "AddNode";
        },
        isSubtract() {
          return this.node.nodeType === "SubtractNode";
        },
        isMultiply() {
          return this.node.nodeType === "MultiplyNode";
        },
        isDivide() {
          return this.node.nodeType === "DivideNode";
        },
        isNegative() {
          return this.node.nodeType === "NegativeNode";
        },
        isNamedLocation() {
          return this.node.nodeType === "NamedLocationNode";
        },
        isNextLocation() {
          return this.node.nodeType === "NextLocationNode";
        },
        isProgramExitLocation() {
          return this.node.nodeType === "ProgramTerminalLocationNode";
        },
        isLoopBreakLocation() {
          return this.node.nodeType === "ExitIterationScopeLocationNode";
        },
        isLoop() {
          return this.node.nodeType === "TranspilerLoop";
        },
        isListIteration() {
          return this.node.nodeType === "ListIterationTranspilerNode";
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
        @sourceNodeClicked="sourceNodeClicked"
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
        @sourceNodeClicked="sourceNodeClicked"
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
        @sourceNodeClicked="sourceNodeClicked"
    />
    <div>{{ indent }}}</div>
  </div>
  <span :id="nodeID" v-else-if="isSymbolReference">ref("{{ this.node.name }}")
  </span>
  <span :id="nodeID" v-else-if="isIndexReference">indexRef("{{ this.node.name }}",
    <UiIntermediateAstNode
        v-for="child in node.indexes"
        :key="child.id"
        :node="child"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
  </span>
  <span :id="nodeID" v-else-if="isValueOf">valueOf(
    <UiIntermediateAstNode
        :node="node.expression"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
  </span>
  <span :id="nodeID" v-else-if="isNot">not(
    <UiIntermediateAstNode
        :node="node.expression"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
  </span>
  <div :id="nodeID" v-else-if="isPrint"
       @click="sourceElementClicked(node)">
    {{ indent }}print(
    <UiIntermediateAstNode
        v-for="child in node.operands"
        :key="child.id"
        :node="child"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    ...
    )
  </div>
  <div :id="nodeID" v-else-if="isSet">
    {{ indent }}set(
    <UiIntermediateAstNode
        :node="node.destination"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />,
    <UiIntermediateAstNode
        :node="node.source"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
  </div>
  <div :id="nodeID" v-else-if="isJump"
       @click="sourceElementClicked(node)"
  >
    {{ indent }}<span class="jump">jump(
    <UiIntermediateAstNode
        :node="node.start"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
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
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.condition"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
  </span>
  <div :id="nodeID" v-else-if="isPlaceholder">
    {{ indent }}...
  </div>
  <span :id="nodeID" v-else-if="isPrimitiveReference">prim({{ this.node.value.value }})
  </span>
  <span :id="nodeID" v-else-if="isNestedCondition">(<UiIntermediateAstNode
      :node="node.expression"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />)
  </span>
  <span :id="nodeID" v-else-if="isOr">or(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isAnd">and(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isEqualTo">eq(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isGreaterThan">gt(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isGreaterThanOrEqualTo">gte(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
    />)
  </span>
  <span :id="nodeID" v-else-if="isLessThan">lt(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isLessThanOrEqualTo">lte(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  />,
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isAdd">(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  /> +
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isSubtract">(<UiIntermediateAstNode
      :node="node.minuend"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  /> -
    <UiIntermediateAstNode
        :node="node.subtrahend"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isMultiply">(<UiIntermediateAstNode
      :node="node.lhs"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  /> *
    <UiIntermediateAstNode
        :node="node.rhs"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isDivide">(<UiIntermediateAstNode
      :node="node.dividend"
      :depth="depth"
      @sourceNodeClicked="sourceNodeClicked"
  /> /
    <UiIntermediateAstNode
        :node="node.divisor"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isNegative">(-
    <UiIntermediateAstNode
        :node="node.divisor"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />)
  </span>
  <span :id="nodeID" v-else-if="isExponent">(
    <UiIntermediateAstNode
        :node="node.basis"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    /> ^
    <UiIntermediateAstNode
        :node="node.exponent"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
  </span>
  <span :id="nodeID" v-else-if="isNamedLocation">loc("{{node.name}}")
  </span>
  <span :id="nodeID" v-else-if="isNextLocation">nextLoc()
  </span>
  <span :id="nodeID" v-else-if="isProgramExitLocation">programExitLoc()
  </span>
  <span :id="nodeID" v-else-if="isLoopBreakLocation">break()
  </span>
  <div :id="nodeID" v-else-if="isIfBlock">
    {{ indent }}if (
    <UiIntermediateAstNode
        :node="node.condition"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
    <UiIntermediateAstNode
        :node="node.ifThenBlock"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    {{ indent }}else
    <UiIntermediateAstNode
        :node="node.ifElseBlock"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
  </div>
  <div :id="nodeID" v-else-if="isLoop">
    {{ indent }}loop(
    <UiIntermediateAstNode
        :node="node.terminateCondition"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
    <UiIntermediateAstNode
        :node="node.body"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
  </div>
  <div :id="nodeID" v-else-if="isListIteration">
    {{ indent }}list_loop(
    <UiIntermediateAstNode
        :node="node.iterable"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
    )
    <UiIntermediateAstNode
        :node="node.body"
        :depth="depth"
        @sourceNodeClicked="sourceNodeClicked"
    />
  </div>
  <span :id="nodeID" v-else :class="{ast_TOP: isTop, ast_BOTTOM: isBottom}">({{ node.nodeType }}
    <UiIntermediateAstNode
        v-for="child in node.childTranspilerNodes"
        :key="child.id"
        :node="child"
        :depth="nextLevel"
        @sourceNodeClicked="sourceNodeClicked"
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
