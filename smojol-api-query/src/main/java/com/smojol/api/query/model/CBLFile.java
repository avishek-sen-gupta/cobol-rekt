package com.smojol.api.query.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;

/**
 * Représentation d'un fichier COBOL avec ses métadonnées et AST
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CBLFile {

    @JsonProperty("name")
    private String name;

    @JsonProperty("path")
    private String path;

    @JsonProperty("program_id")
    private String programId;

    @JsonProperty("size")
    private long size;

    @JsonProperty("lines")
    private int lines;

    @JsonProperty("variables")
    private List<String> variables;

    @JsonProperty("procedures")
    private List<String> procedures;

    @JsonProperty("copybooks")
    private List<String> copybooks;

    /**
     * Liste détaillée des copybooks avec informations extraites
     * Peuplée lors du parsing du champ "text"
     */
    private List<Copybook> copybooksList;

    @JsonProperty("datasets")
    private List<String> datasets;

    @JsonProperty("callees")
    private List<String> callees;

    @JsonProperty("callers")
    private List<String> callers;

    @JsonProperty("jcls")
    private List<String> jcls;

    @JsonProperty("parse_status")
    private ParseStatus parseStatus;

    @JsonProperty("parse_message")
    private String parseMessage;

    @JsonProperty("ast_data")
    private Map<String, Object> astData;

    @JsonProperty("last_modified")
    private long lastModified;

    /**
     * Retourne le chemin relatif vers le fichier AST
     */
    public String getAstFileName() {
        return name + "-aggregated.json";
    }

    /**
     * Vérifie si le fichier est valide
     */
    public boolean isValid() {
        return parseStatus == ParseStatus.SUCCESS && astData != null;
    }

    /**
     * Retourne le nombre de copybooks utilisés
     */
    public int getCopybookCount() {
        return copybooks != null ? copybooks.size() : 0;
    }

    /**
     * Retourne le nombre de datasets utilisés
     */
    public int getDatasetCount() {
        return datasets != null ? datasets.size() : 0;
    }

    /**
     * Retourne le nombre de programmes appelés
     */
    public int getCalleeCount() {
        return callees != null ? callees.size() : 0;
    }

    /**
     * Retourne le nombre de programmes appelants
     */
    public int getCallerCount() {
        return callers != null ? callers.size() : 0;
    }

    /**
     * Vérifie si ce programme utilise un copybook
     */
    public boolean usesCopybook(String copybook) {
        return copybooks != null && copybooks.contains(copybook);
    }

    /**
     * Vérifie si ce programme accède un dataset
     */
    public boolean usesDataset(String dataset) {
        return datasets != null && datasets.contains(dataset);
    }

    /**
     * Vérifie si ce programme appelle un autre programme
     */
    public boolean callsProgram(String program) {
        return callees != null && callees.contains(program);
    }

    /**
     * Vérifie si ce programme est appelé par un autre programme
     */
    public boolean isCalledBy(String program) {
        return callers != null && callers.contains(program);
    }
}
