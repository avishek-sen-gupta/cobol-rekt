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
            console.log(response.data);
            this.projectListing = response.data;
          })
          .catch(function (err) {
            console.log("There was an error: ");
            console.log(err);
          });
    },
    loadIntermediateAST(id) {
      this.$emit("load-ir-ast", id);
    },
    loadIntermediateCFG(id) {
      this.$emit("load-ir-cfg", id);
    }
  }
}
</script>

<template>
  <div class="headered-pane" id="project-listing-pane">
    <div class="pane-heading">
      <span>Projects</span>
      <img id="refresh-projects" alt="Refresh" src="../assets/icons8-refresh-50.png" @click="getProjectListing"
           class="refresh-button"/>
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
              <strong>I-AST</strong>: {{ iast.programName }} / {{ iast.astID }}
            </li>
          </ul>
          <ul>
            <li v-for="icfg in project.cfgListings"
                :key="icfg.cfgID"
                @click="loadIntermediateCFG(icfg.cfgID)">
              <strong>I-CFG</strong>: {{ icfg.programName }} / {{ icfg.cfgID }}
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
  background-color: azure;
  overflow-y: scroll;
}

.refresh-button {
  width: 7%;
  padding-left: 10px;
  height: auto;
  vertical-align: center;
}

.refresh-button:hover {
  transform: scale(1.1); /* Scale down slightly on click */
}

.refresh-button:active {
  transform: scale(0.9); /* Scale down slightly on click */
}
</style>
