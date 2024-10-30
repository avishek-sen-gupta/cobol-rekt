<template>
  <div class="hello">
    <h1>{{ header }}</h1>
    <p>
      Welcome to Cobol-REKT. See <a href="https://github.com/avishek-sen-gupta/cobol-rekt">manual</a> for further instructions.
    </p>
  </div>
</template>

<script>
export default {
  name: 'HelloWorld',
  props: {
    header: String
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
