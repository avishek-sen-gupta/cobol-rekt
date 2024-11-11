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
    loadRawAST(id) {
      this.$emit("load-raw-ast", id);
    },
    loadIntermediateCFG(id) {
      this.$emit("load-ir-cfg", id);
    },
    loadFlowModel(id) {
      this.$emit("load-flow-model", id);
    },
    loadFlowchart(id) {
      this.$emit("load-flowchart", id);
    }
  }
}
</script>

<template>
  <div class="headered-pane" id="project-listing-pane">
    <div class="pane-heading" style="display: flex; align-items: center;">
      <span>Projects</span>
      <img id="refresh-projects" alt="Refresh" src="../assets/icons8-refresh-50.png" @click="getProjectListing"
           class="refresh-button"/>
    </div>
    <div id="project-listing-details" class="clt">
      <ul>
        <li v-for="project in projectListing"
            :key="project.projectID">
          Project ID: {{ project.projectID }}
          <ul>
            <li><strong>AST</strong>
              <ul>
                <li v-for="rawAST in project.rawASTListings"
                    :key="rawAST.astID"
                    @click="loadRawAST(rawAST.rawASTID)">
                  <span class="hoverable">{{ rawAST.programName }} / {{ rawAST.rawASTID }}</span>
                </li>
              </ul>
            </li>
            <li><strong>AST</strong>
              <ul>
                <li v-for="iast in project.astListings"
                    :key="iast.astID"
                    @click="loadIntermediateAST(iast.astID)">
                  <span class="hoverable">{{ iast.programName }} / {{ iast.astID }}</span>
                </li>
              </ul>
            </li>
            <li><strong>CFG</strong>
              <ul>
                <li v-for="icfg in project.cfgListings"
                    :key="icfg.cfgID"
                    @click="loadIntermediateCFG(icfg.cfgID)">
                  <span class="hoverable">{{ icfg.programName }} / {{ icfg.cfgID }}</span>
                </li>
              </ul>
            </li>
            <li><strong>Flow Model</strong>
              <ul>
                <li v-for="flowModel in project.unifiedFlowModelListings"
                    :key="flowModel.flowModelID"
                    @click="loadFlowModel(flowModel.flowModelID)">
                  <span class="hoverable">{{ flowModel.programName }} / {{ flowModel.flowModelID }}</span>
                </li>
              </ul>
            </li>
            <li><strong>Flowchart</strong>
              <ul>
                <li v-for="flowchart in project.flowchartListings"
                    :key="flowchart.flowchartID"
                    @click="loadFlowchart(flowchart.flowchartID)">
                  <span class="hoverable">{{ flowchart.programName }} / {{ flowchart.flowchartID }}</span>
                </li>
              </ul>
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
  overflow-y: auto;
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
