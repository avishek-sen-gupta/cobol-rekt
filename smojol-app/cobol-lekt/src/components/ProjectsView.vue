<template>
  <div id="project-listing-pane">
    <h3>Projects</h3>
    <div id="project-listing-details">
      <div v-for="project in projectListing"
           :key="project.projectID">
        Project ID: {{ project.projectID }}
        <ul>
          <li v-for="iast in project.astListings"
               :key="iast.astID">
            &emsp;I-AST ID: {{ iast.programName }} / {{ iast.astID }}
          </li>
        </ul>
      </div>
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
    this.getProjectListing();
  },
  methods: {
    getProjectListing() {
      axios.get("/api/projects")
          .then(response => {
            console.log(response);
            this.projectListing = response.data;
          });
    },
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
