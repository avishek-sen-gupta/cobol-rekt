<script>
import HelloWorld from './components/HelloWorld.vue'
import {ref} from "vue";

export default {
  name: 'App',
  components: {
    HelloWorld
  },
  setup() {
    const codeArea = ref("Here's some code");
    function updateText1() {
      codeArea.value = "BLAH BLAH";
    }

    return {codeArea, updateText1};
  }

}

</script>

<template>
  <img alt="Vue logo" src="./assets/cobol-rekt-banner.png">
  <div>What {{ codeArea }}</div>
  <HelloWorld header="Welcome to this amazing app"/>
  <LeTextArea v-model="codeArea" rows="69" columns="100"/>
  <LeButton label="Le Button" @click="updateText1"></LeButton>
</template>


<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
