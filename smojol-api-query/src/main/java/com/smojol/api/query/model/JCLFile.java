package com.smojol.api.query.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Représentation d'un fichier JCL (Job Control Language)
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class JCLFile {

    @JsonProperty("name")
    private String name;

    @JsonProperty("path")
    private String path;

    @JsonProperty("job_name")
    private String jobName;

    @JsonProperty("size")
    private long size;

    @JsonProperty("lines")
    private int lines;

    @JsonProperty("programs")
    private List<String> programs;

    @JsonProperty("datasets")
    private List<String> datasets;

    @JsonProperty("steps")
    private List<JCLStep> steps;

    @JsonProperty("parse_status")
    private ParseStatus parseStatus;

    @JsonProperty("parse_message")
    private String parseMessage;

    @JsonProperty("jcl_data")
    private Map<String, Object> jclData;

    @JsonProperty("last_modified")
    private long lastModified;

    /**
     * Retourne le chemin relatif vers le fichier AST JCL
     */
    public String getJclFileName() {
        return name + "-aggregated.json";
    }

    /**
     * Vérifie si le fichier est valide
     */
    public boolean isValid() {
        return parseStatus == ParseStatus.SUCCESS && jclData != null;
    }

    /**
     * Retourne le nombre de programmes utilisés
     */
    public int getProgramCount() {
        return programs != null ? programs.size() : 0;
    }

    /**
     * Retourne le nombre de datasets utilisés
     */
    public int getDatasetCount() {
        return datasets != null ? datasets.size() : 0;
    }

    /**
     * Retourne le nombre de steps JCL
     */
    public int getStepCount() {
        return steps != null ? steps.size() : 0;
    }

    /**
     * Vérifie si ce JCL utilise un programme
     */
    public boolean usesProgram(String program) {
        return programs != null && programs.contains(program);
    }

    /**
     * Vérifie si ce JCL accède un dataset
     */
    public boolean usesDataset(String dataset) {
        return datasets != null && datasets.contains(dataset);
    }

    /**
     * Représentation d'une étape JCL
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class JCLStep {
        @JsonProperty("name")
        private String name;

        @JsonProperty("program")
        private String program;

        @JsonProperty("datasets")
        private List<String> datasets;

        @JsonProperty("dd_statements")
        private List<DdStatement> ddStatements;

        @JsonProperty("parameters")
        private Map<String, String> parameters;
    }
}
