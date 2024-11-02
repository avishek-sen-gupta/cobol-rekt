<template>
  <div id="project-listing-pane">
    <div class="pane-heading">Projects
      <img id="refresh-projects" alt="Refresh" src="../assets/MdRefreshCircle.svg" @click="getProjectListing"
           style="align-self: center; width: 5%; height: auto"/>
    </div>
    <div id="project-listing-details">
      <ul>
        <li v-for="project in projectListing"
             :key="project.projectID">
          Project ID: {{ project.projectID }}
          <ul>
            <li v-for="iast in project.astListings"
                :key="iast.astID"
                @click="loadIntermediateAST(iast.astID)">
              I-AST ID: {{ iast.programName }} / {{ iast.astID }}
            </li>
          </ul>
        </li>
      </ul>
    </div>
  </div>
</template>

<script>
import axios from "axios";

export default {
  name: 'ProjectsView',
  data() {
    return {projectListing: null};
  },
  mounted() {
    // this.getProjectListing();
  },
  methods: {
    getProjectListing() {
      axios.get("/api/projects")
          .then(response => {
            console.log(response);
            this.projectListing = response.data;
          });
    },
    loadIntermediateAST(id) {
      console.log("Emitting event : " + id);
      this.$emit("load-ir-ast", id);
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}


a {
  color: #42b983;
}

#project-listing-details {
  width: 350px;
  height: 286px;
  border: 1px solid;
  background-color: azure;
  overflow-y: scroll;
}
</style>
