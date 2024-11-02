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
            this.projectListing = response.data;
          })
          .catch(function (err) {
            console.log("There was an error: ");
            console.log(err);
          });
    },
    loadIntermediateAST(id) {
      this.$emit("load-ir-ast", id);
    }
  }
}
</script>

<template>
  <div class="headered-pane" id="project-listing-pane">
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


<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
a {
  color: #42b983;
}

#project-listing-details {
  height: 100%;
  border: 1px solid;
  background-color: azure;
  overflow-y: scroll;
}
</style>
